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
import           Control.Monad.Trans.Resource
import           Data.ByteString             (ByteString)
import           Data.ByteString.Builder     (hPutBuilder)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary         as Conduit
import qualified Data.Conduit.Zlib           as Conduit
import           Data.Foldable               (foldMap)
import           Data.List                   (intersperse)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import           Data.Word
import           Filesystem.Path.CurrentOS   ((</>))
import qualified Filesystem.Path.CurrentOS   as Path
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
import           System.APT.Index            (Index)
import qualified System.APT.Index            as Index
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package          as Pkg
import           System.APT.Store            (Store)
import qualified System.APT.Store            as Store
import           System.APT.Types
import           System.Directory
import           System.IO
import qualified System.Logger               as Log
import           System.LoggerT              hiding (Error)

default (ByteString)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Bucket
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

    <*> bucketOption
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
        <> help "Maximum number of packages to process concurrently. [default: 6]"
        <> value 6
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
    , appStore   :: Store
    , appIndex   :: Index
    , appLogger  :: Logger
    , appLock    :: MVar ()
    }

newEnv :: Options -> IO Env
newEnv o@Options{..} = do
    s <- Store.new optKey optVersions <$> loadEnv optDebug
    Env o s <$> Index.new s <*> newLogger <*> newMVar ()

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
runApp e = (`runReaderT` e) . unApp

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
    runSettings (settings e) (middleware e) `finally` closeEnv e
  where
    middleware e = logRequest (appLogger e) . GZip.gzip GZip.def $ handler e
    handler    e = runHandler e . route (prepare routes)

    settings e =
          setHost (fromString optHost)
        . setPort (fromIntegral optPort)
        . setBeforeMainLoop (logStart $ appLogger e)
        . setOnException (logException $ appLogger e)
        . setOnExceptionResponse (const serverError)
        . setTimeout 60
        $ defaultSettings

    logStart l = do
        Log.info l $ msg "Apteryx server starting..."
        Log.info l $ msg ("Listening on " +++ optHost +++ ':' +++ optPort)

    logException l _ x = Log.err l $ msg (BS.pack $ show x)

    serverError = plain status500 "server-error\n"

routes :: Routes a (EitherT Error App) ()
routes = do
    get   "/i/status" (const $ return blank) true
    head  "/i/status" (const $ return blank) true

    patch "/packages/:arch/:name/:vers" reindex $
            capture "arch"
        .&. capture "name"
        .&. capture "vers"

    post  "/packages" (const rebuild) true

    get   "/Packages"    (const $ index "Packages") true
    get   "/Packages.gz" (const $ index "Packages.gz") true
  where
    patch = addRoute "PATCH"

reindex :: Arch ::: Name ::: Vers -> Handler
reindex (arch ::: name ::: vers) = do
--    check if file exists in S3, return 404 if not found
    return blank

index :: Path -> Handler
index file = do
    path <- Path.encodeString . (</> file) . optWWW <$> asks appOptions
    sayT "index" "{}" [path]
    return $ responseFile status200 [] path Nothing

rebuild :: Handler
rebuild = return $ plain status500 "not-implemented\n"

  --   e <- ask
  --   p <- liftIO . isEmptyMVar $ appLock e
  --   when p $ go e
  --   return $ rs p
  -- where
  --   rs True  = plain status200 "rebuild-in-progress\n"
  --   rs False = plain status202 "starting-rebuild\n"

  --   go Env{..} = fork . lock . temp $ \path hd -> do
  --       let src = Path.encodeString path

  --       say appLogger "rebuild" "Starting rebuild of {}..." [src]

  --       hSetBinaryMode hd True
  --       hSetBuffering hd (BlockBuffering Nothing)

  --       either throwM return =<< AWS.runAWSEnv appEnv (do
  --           xs <- S3.entries appLogger "rebuild" optKey optVersions
  --           liftIO $ parForM optN xs metadata (either throwM (append hd)))

  --       hClose hd <* say appLogger "rebuild" "Closed {}" [src]
  --       copyFile src pkg <* say appLogger "rebuild" "Copied {}" [pkg]

  --       runResourceT $ Conduit.sourceFile src
  --           =$ Conduit.gzip
  --           $$ Conduit.sinkFile gzip
  --       say appLogger "rebuild" "Compressed {}" [gzip]
  --     where
  --       Options{..} = appOptions

  --       fork = void . liftIO . forkIO
  --       lock = withMVar appLock . const
  --       temp = withTempFile optTemp ".pkg"

  --       pkg  = Path.encodeString (optWWW </> "Packages")
  --       gzip = Path.encodeString (optWWW </> "Packages.gz")

  --       metadata = AWS.runAWSEnv appEnv
  --           . mapM (S3.metadata appLogger "meta" . entKey)

  --       append hd xs = do
  --           let ys = reverse xs
  --               n  = maybe "N/A" (Text.decodeUtf8 . pkgPackage) (listToMaybe ys)
  --               vs = Text.decodeUtf8 . BS.intercalate ", " $ map pkgVersion ys
  --           say appLogger "index" "Indexing {} {}" [n, vs]
  --           hPutBuilder hd $ foldMap (\x -> Pkg.toBuilder x <> "\n") ys

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
        Error        s   -> (status500, "server-error",    LBS.pack s)
        Exception    ex  -> (status500, "server-error",    LBS.pack $ show ex)

    encode = LBS.fromStrict . Text.encodeUtf8

logRequest :: Logger -> Middleware
logRequest l app rq = line *> app rq
  where
    line = Log.debug l . msg
          $ show (remoteHost rq)
        +++ ", "
        +++ requestMethod rq
        +++ ' '
        +++ rawPathInfo rq
        +++ rawQueryString rq
        +++ show (httpVersion rq)
