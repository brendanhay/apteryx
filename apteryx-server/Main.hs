{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Module      : Main
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.ThreadPool
import           Control.Monad
import           Control.Monad.Catch         hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString             (ByteString)
import           Data.ByteString.Builder     (hPutBuilder)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Foldable               (foldMap)
import           Data.List                   (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text.Encoding          as Text
import           Data.Word
import           Filesystem.Path.CurrentOS   ((</>))
import qualified Filesystem.Path.CurrentOS   as Path
import qualified Network.APT.S3              as S3
import           Network.AWS                 (Credentials(..), AWSEnv)
import qualified Network.AWS                 as AWS
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error, err, hd)
import           Network.Wai.Routing         hiding (options)
import           Options.Applicative
import           Prelude                     hiding (concatMap, head)
import           System.APT.IO
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package          as Pkg
import           System.APT.Types
import           System.Directory
import           System.IO
import qualified System.Logger               as Log
import           System.LoggerT

default (ByteString)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Key
    , optTemp     :: !Path
    , optWWW      :: !Path
    , optN        :: !Int
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq)

options :: Parser Options
options = Options
    <$> strOption
         ( long "host"
        <> metavar "HOST"
        <> help "Hostname or address to bind on. [default: 127.0.0.1]"
        <> value "127.0.0.1"
         )

    <*> option
         ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Port number to listen on. [default: 8080]"
        <> value 8080
         )

    <*> keyOption
         ( long "key"
        <> short 'k'
        <> metavar "BUCKET/PREFIX"
        <> help "Source S3 bucket and optional prefix to traverse for packages. [required]"
         )

    <*> pathOption
         ( long "tmp"
        <> short 't'
        <> metavar "PATH"
        <> help "Temporary directory for building new Package indexes. [default: /tmp]"
        <> value "/tmp"
         )

    <*> pathOption
         ( long "www"
        <> short 'w'
        <> metavar "PATH"
        <> help "Directory to serve the generated Packages index from. [default: www]"
        <> value "www"
         )

    <*> option
         ( long "concurrency"
        <> short 'c'
        <> metavar "INT"
        <> help "Maximum number of packages to process concurrently. [default: 10]"
        <> value 10
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Maximum number of most recent package versions to retain. [default: 3]"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

data Env = Env
    { appOptions :: !Options
    , appEnv     :: AWSEnv
    , appLogger  :: Logger
    , appLock    :: MVar ()
    }

newEnv :: Options -> IO Env
newEnv opts = do
    e <- runEitherT $ AWS.loadAWSEnv AuthDiscover (optDebug opts)
    Env opts <$> either error return e
             <*> newLogger
             <*> newMVar ()

closeEnv :: Env -> IO ()
closeEnv = Log.close . appLogger

newtype App a = App { unApp :: ReaderT Env IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadReader Env
             )

runApp :: Env -> App a -> IO a
runApp env = (`runReaderT` env) . unApp

instance MonadLogger App where
    logger = asks appLogger

instance MonadLogger (EitherT e App) where
    logger = lift logger
    prefix = lift prefix

type Handler = EitherT Error App Response

runHandler :: Env -> Handler -> IO Response
runHandler e h = runApp e (eitherT onError return h)

main :: IO ()
main = parseOptions options >>= serve

serve :: Options -> IO ()
serve o@Options{..} = do
    ensureExists optWWW
    e <- newEnv o
    runSettings (settings e) (pipeline e) `finally` closeEnv e
  where
    pipeline e = GZip.gzip GZip.def (handler e)
    handler  e = runHandler e . route (prepare routes)
    settings e =
          setHost (fromString optHost)
        . setPort (fromIntegral optPort)
        . setBeforeMainLoop (logStart $ appLogger e)
        . setOnException (logException $ appLogger e)
        . setOnExceptionResponse (const serverError)
        . setTimeout 60
        $ defaultSettings

    logStart l = do
        Log.info l $ msg "Apteryx starting..."
        Log.info l $ msg ("Listening on " +++ optHost +++ ':' +++ optPort)

    logException l _ x = Log.err l $ msg (BS.pack $ show x)

    serverError = plain status500 "server-error\n"

routes :: Routes a (EitherT Error App) ()
routes = do
    get   "/i/status" (const $ return blank) true
    head  "/i/status" (const $ return blank) true

    post  "/packages" (const rebuild) true

--     patch "/packages/:arch/:name/:vers" reindex $
--         capture "arch" .&. capture "name" .&. capture "vers"

-- --    get   "/packages/:arch/:package" (const $ return blank) true
--     -- get   "/packages/:arch/:package/:vers" (const $ return blank) true

  -- where
  --   patch = addRoute "PATCH"

-- reindex :: Arch ::: Text ::: Text -> Handler
-- reindex (arch ::: name ::: vers) = do
--     debug $ field "reindex" (arch +++ "/" +++ name +++ "/" +++ vers)
--     return blank

rebuild :: Handler
rebuild = do
    p <- asks appLock >>= liftIO . isEmptyMVar
    if p
        then do
            sayT_ "rebuild" "Rebuild already in progress."
            return (plain status200 "rebuild-in-progress\n")
        else do
            ask >>= void . liftIO . forkIO . worker
            return (plain status202 "starting-rebuild\n")

worker :: Env -> IO ()
worker Env{..} = withMVar appLock . const .
    withTempFile optTemp ".pkg" $ \path hd -> do
        let src  = Path.encodeString path
            dest = Path.encodeString (optWWW </> "Packages")
            say' = say appLogger

        say_ appLogger "worker" "Starting rebuild..."

        hSetBinaryMode hd True
        hSetBuffering hd (BlockBuffering Nothing)

        say' "worker" "Opened {}" [src]

        rs <- AWS.runAWSEnv appEnv $ do
            xs <- S3.entries appLogger "worker" optKey optVersions
            liftIO $ parForM optN xs metadata (either throwM (append hd))
        either throwM return rs

        hClose hd
        say' "gather" "Closed {}" [src]

        copyFile src dest
        say' "worker" "Wrote {}" [dest]

        say_ appLogger "worker" "Rebuild complete."
  where
    Options{..} = appOptions

    metadata = AWS.runAWSEnv appEnv . mapM (S3.metadata appLogger "scatter" . entKey)

    append hd = hPutBuilder hd . foldMap (\x -> Pkg.toBuilder x <> "\n") . sort

plain :: Status -> LBS.ByteString -> Response
plain code = responseLBS code []

blank :: Response
blank = plain status200 ""

onError :: Error -> App Response
onError e = case statusCode code of
    c | c >= 500  -> err errField
      | c >= 400  -> debug errField
      | otherwise -> return ()
    >> return (responseLBS code [] $ LBS.fromStrict lbl <> "\n")
  where
    errField = field lbl ("\"" +++ line +++ "\"")

    (code, lbl, line) = case e of
        MissingField m   -> (status409, "invalid-package", encode m)
        InvalidField m   -> (status413, "invalid-package", encode m)
        ShellError _ _ m -> (status500, "invalid-package", m)
        Exception ex     -> (status500, "server-error",    LBS.pack $ show ex)

    encode = LBS.fromStrict . Text.encodeUtf8
