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
import           Control.Monad
import           Control.Monad.Catch         hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Word
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error, err, hd)
import           Network.Wai.Routing         hiding (options)
import           Options.Applicative
import           Prelude                     hiding (concatMap, head)
import           System.APT.IO
import qualified System.APT.Index            as Index
import           System.APT.Log
import           System.APT.Options
import           System.APT.Store            (Store)
import qualified System.APT.Store            as Store
import           System.APT.Types
import qualified System.Logger               as Log
import           System.LoggerT              hiding (Error)

default (Builder)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Bucket
    , optTemp     :: !FilePath
    , optWWW      :: !FilePath
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq)

-- - Additional flags
--   Origin
--   Label
--   Codename/Distribution
--   Valid-Until
--   Description

-- - InRelease signing

-- - Add distribution
--   Requires uploader being able to upload to specific distros, or none (equates to all)
--   Need to write out the indicies per distro

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

    <*> strOption
         ( long "tmp"
        <> short 't'
        <> metavar "PATH"
        <> help "Temporary directory for building new Package indexes. [default: /tmp]"
        <> value "/tmp"
         )

    <*> strOption
         ( long "www"
        <> short 'w'
        <> metavar "PATH"
        <> help "Directory to serve the generated Packages index from. [default: www]"
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
    , appStore   :: !Store
    , appLogger  :: !Logger
    , appLock    :: MVar ()
    }

newEnv :: Options -> IO Env
newEnv o@Options{..} = Env o
    <$> (Store.new optKey optVersions <$> discoverAWSEnv optDebug)
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
    say_ "aws" "Discovering credentials..."
    e@Env{..} <- newEnv o
    say_ "index" "Syncing package database..."
    Index.latest (spec o) appStore >>= Index.generate optTemp optWWW
    say_ "server" "Apteryx server starting..."
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
    get  "/i/status"    (const $ return blank) true
    head "/i/status"    (const $ return blank) true

    post "/packages"    (const triggerRebuild) true

    get  "binary-:arch/Packages"    (const getIndex) true


    get  "/Packages.xz" (const getIndex) true

    -- addRoute "PATCH" "/packages/:arch/:name/:vers" addEntry $
    --         capture "arch"
    --     .&. capture "name"
    --     .&. capture "vers"

-- addEntry :: Arch ::: Name ::: Vers -> Handler
-- addEntry (a ::: n ::: v) = do
    -- i <- asks appIndex
    -- m <- catchError $ Index.sync i >> Index.lookup a n v i
    -- return $
    --     if isJust m
    --         then plain status200 "success\n"
    --         else plain status404 "not-found\n"

triggerRebuild :: Handler
triggerRebuild = do
    Env{..} <- ask

    let o@Options{..} = appOptions

    p <- catchError . sync appLock $
        Index.latest (spec o) appStore >>= Index.generate optTemp optWWW

    respond $ if p
       then ("Triggering rebuild...", status202, "triggered-rebuild\n")
       else ("In progress.", status200, "rebuild-in-progress\n")

  where
    respond (m, st, r) = sayT_ "rebuild" m >> return (plain st r)

    sync l f = do
        m <- tryTakeMVar l
        maybe (return False)
              (\v -> do
                  void $ f `forkFinally` const (putMVar l v)
                  return True)
              m

getIndex :: Handler
getIndex = undefined
--    withReadLock

    -- i <- asks appIndex
    -- let path = Path.encodeString (Index.path i)
    -- return $ responseFile status200 [] path Nothing

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

spec Options{..} = mkInRelease "origin" "label" "codename" "description"
