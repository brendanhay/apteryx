{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Control.Monad
import           Control.Monad.Catch         hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import           Data.Word
import           Network.AWS
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error)
import           Network.Wai.Routing         hiding (options)
import           Options.Applicative
import           Prelude                     hiding (head)
import           System.APT.IO
import           System.APT.Options
import           System.APT.Package
import           System.APT.Types
import qualified System.Logger               as Log
import           System.LoggerT
import qualified System.LoggerT              as LogT

default (ByteString)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Key
    , optWWW      :: !Path
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
         ( long "dir"
        <> metavar "PATH"
        <> help "Directory to serve the generated Packages index from. [default: ./www]"
        <> value "www"
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
    e <- runEitherT $ loadEnv AuthDiscover (optDebug opts)
    Env opts <$> either error return e
             <*> Log.new Log.defSettings
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

-- instance MonadThrow (EitherT e App) where
--     throwM = lift . throwM

-- instance MonadCatch (EitherT e App) where
--     catch m f = EitherT $
--         runEitherT m `catch` \e -> runEitherT (f e)

--     mask a = EitherT $
--         mask $ \u -> runEitherT (a $ mapEitherT u)

--     uninterruptibleMask a = EitherT $
--         uninterruptibleMask $ \u -> runEitherT (a $ mapEitherT u)

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

    serverError = responseLBS status500 [] "server-error\n"

routes :: Routes a (EitherT Error App) ()
routes = do
    post  "/packages" (const rebuild) true

--     patch "/packages/:arch/:name/:vers" reindex $
--         capture "arch" .&. capture "name" .&. capture "vers"

-- --    get   "/packages/:arch/:package" (const $ return blank) true
--     -- get   "/packages/:arch/:package/:vers" (const $ return blank) true

--     get   "/i/status" (const $ return blank) true
--     head  "/i/status" (const $ return blank) true
  -- where
  --   patch = addRoute "PATCH"

-- reindex :: Arch ::: Text ::: Text -> Handler
-- reindex (arch ::: name ::: vers) = do
--     LogT.debug $ field "reindex" (arch +++ "/" +++ name +++ "/" +++ vers)
--     return blank

rebuild :: Handler
rebuild  = do
    p <- asks appLock >>= liftIO . isEmptyMVar
    if p then inprogress else invoke
  where
    inprogress :: Handler
    inprogress = do
        LogT.debug $ field "rebuild" "Rebuild already in progress."
        return (responseLBS status200 [] "rebuild-in-progress\n")

    invoke :: Handler
    invoke = do
        forkWorker
        return (responseLBS status200 [] "starting-rebuild\n")

    forkWorker :: EitherT Error App ()
    forkWorker = do
        Env{..} <- ask
        void . liftIO
             . forkIO
             . void
             $ flip runEnv appEnv $ do
                 Log.debug appLogger $ field "rebuild-worker" "Starting rebuild..."
                 liftIO . withMVar appLock $ const (threadDelay 15000000)
                 Log.debug appLogger $ field "rebuild-worker" "Rebuild complete."

blank :: Response
blank = responseLBS status200 [] ""

onError :: Error -> App Response
onError e = case statusCode code of
    c | c >= 500  -> LogT.err errField
      | c >= 400  -> LogT.debug errField
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
