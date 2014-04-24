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
import           Data.ByteString.Builder
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
import qualified Network.Wai.Middleware.RequestLogger (logStdout)
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
    { appIndex  :: Index
    , appLogger :: Logger
    }

newEnv :: Options -> IO Env
newEnv Options{..} = do
    s <- Store.new optKey optVersions <$> loadEnv optDebug
    Env <$> Index.new optN optWWW optTemp s <*> newLogger

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

instance MonadThrow (EitherT e App) where
    throwM = lift . throwM

instance MonadCatch (EitherT e App) where
    catch m f = EitherT $
        runEitherT m `catch` \e -> runEitherT (f e)

    mask a = EitherT $
        mask $ \u -> runEitherT (a $ mapEitherT u)

    uninterruptibleMask a = EitherT $
        uninterruptibleMask $ \u -> runEitherT (a $ mapEitherT u)

type Handler = EitherT Error App Response

runHandler :: Env -> Handler -> IO Response
runHandler e h = runApp e (eitherT onError return h)

main :: IO ()
main = parseOptions options >>= serve

serve :: Options -> IO ()
serve o@Options{..} = do
    e@Env{..} <- newEnv o
    Log.info appLogger $ msg "Apteryx server starting..."
    Log.info appLogger $ msg "Syncing package database..."
    Index.sync appIndex
    Log.info appLogger $ msg "Sync complete."
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

    logStart l =
        Log.info l $ msg ("Listening on " +++ optHost +++ ':' +++ optPort)

    logException l _ = Log.err l . msg . show

    serverError = plain status500 "server-error\n"

routes :: Routes a (EitherT Error App) ()
routes = do
    get   "/i/status" (const $ return blank) true
    head  "/i/status" (const $ return blank) true

    patch "/packages/:arch/:name/:vers" addEntry $
            capture "arch"
        .&. capture "name"
        .&. capture "vers"

    post  "/packages" (const triggerRebuild) true
    get   "/Packages" (const getIndex) true
  where
    patch = addRoute "PATCH"

addEntry :: Arch ::: Name ::: Vers -> Handler
addEntry (a ::: n ::: v) = do
    i <- asks appIndex
    p <- catchErrorT $ Index.sync i >> Index.member a n v i
    return $
        if p
            then plain status200 "success\n"
            else plain status404 "not-found\n"

triggerRebuild :: Handler
triggerRebuild = do
    i <- asks appIndex
    p <- catchErrorT $ Index.trySync i
    return $
        if p
           then plain status202 "rebuilding-index\n"
           else plain status200 "in-progress\n"

getIndex :: Handler
getIndex = do
    i <- asks appIndex
    catchErrorT $ Index.syncIfMissing i
    let path = Path.encodeString (Index.path i)
    return $ responseFile status200 [] path Nothing

plain :: Status -> LBS.ByteString -> Response
plain code = responseLBS code []

blank :: Response
blank = plain status200 ""

onError :: Error -> App Response
onError e = case statusCode code of
    c | c >= 500  -> err $ msg line
      | c >= 400  -> debug $ msg line
      | otherwise -> return ()
    >> return (responseLBS code [] $ line <> "\n")
  where
    (code, line) = case e of
        Error   c bs -> (c, toLazyByteString $ bytes bs)
        Exception ex -> (status500, LBS.pack $ show ex)

logRequest :: Logger -> Middleware
logRequest l app rq = do
    rs <- app rq
    Log.debug l . msg $ requestMethod rq
        +++ ' '
        +++ rawPathInfo rq
        +++ rawQueryString rq
        +++ ' '
        +++ show (httpVersion rq)
        +++ " ["
        +++ statusCode (responseStatus rs)
        +++ "] \""
        +++ fromMaybe mempty (lookup "user-agent" $ requestHeaders rq)
        +++ "\" \""
        +++ fromMaybe mempty (lookup "referer" $ requestHeaders rq)
        +++ '"'
    return rs
