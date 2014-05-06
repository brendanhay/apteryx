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
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
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
import           System.Directory
import           System.FilePath
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
    <*> pure getLogger
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
        $ defaultSettings

    logStart l =
        Log.info l $ msg ("Listening on " +++ optHost +++ ':' +++ optPort)

    logException l _ = Log.err l . msg . show

    serverError = plain status500 "server-error\n"

routes :: Routes a (EitherT Error App) ()
routes = do
    post "/packages" (const triggerRebuild) true

    get  "/packages/:arch/:name/:vers" getPackage $
            capture "arch"
        .&. capture "name"
        .&. capture "vers"

    patch "/packages/:arch/:name/:vers" addPackage $
            capture "arch"
        .&. capture "name"
        .&. capture "vers"

    get  "/InRelease" (const $ getIndex "InRelease") true
    get  "/Release"   (const $ getIndex "Release") true

    get  "/:arch/Packages"    (getArch "Packages")    $ capture "arch"
    get  "/:arch/Packages.gz" (getArch "Packages.gz") $ capture "arch"
    get  "/:arch/Release"     (getArch "Release")     $ capture "arch"

    get  "/i/status" (const $ return blank) true
    head "/i/status" (const $ return blank) true

getPackage :: Arch ::: Name ::: Vers -> Handler
getPackage (a ::: n ::: v) = do
    Options{..} <- asks appOptions

    s <- asks appStore
    t <- addUTCTime (fromInteger 10) <$> liftIO getCurrentTime
    u <- Store.presign (Entry n v a ()) t s

    return $! responseLBS status302 [("Location", u)] mempty

addPackage :: Arch ::: Name ::: Vers -> Handler
addPackage (a ::: n ::: v) = do
    s <- asks appStore
    let x' = Entry n v a ()
        x  = Store.objectKey x' s
    sayT name "Searching for {}" [x]
    Store.metadata x' s >>= respond x . isJust
  where
    respond x True = do
        sayT name "Found package {}, rebuilding local index" [x]
        ask >>= \e -> catchError . withMVar (appLock e) $ const (sync e)
        sayT name "Index rebuild complete after adding {}" [x]
        return $ plain status200 "index-rebuilt\n"

    respond x False = do
        sayT name "Unable to find package {}" [x]
        return $ plain status404 "not-found\n"

    name = "add-package"

triggerRebuild :: Handler
triggerRebuild =
    ask >>= \e -> catchError (lock (appLock e) (sync e)) >>= respond
  where
    respond True = do
        sayT_ name "Triggering local index rebuild"
        return $ plain status202 "triggered-rebuild\n"

    respond False = do
        sayT_ name "Rebuild already in progress"
        return $ plain status200 "rebuild-in-progress\n"

    name = "rebuild"

    lock l f = do
        m <- tryTakeMVar l
        maybe (return False)
              (\v -> do
                  void $ f `forkFinally` const (putMVar l v)
                  return True)
              m

getArch :: FilePath -> Arch -> Handler
getArch f a = getIndex ("binary-" ++ show a </> f)

getIndex :: FilePath -> Handler
getIndex f = do
    path <- (</> f) <$> asks (optWWW . appOptions)
    p    <- catchError (doesFileExist f)
    return $ if p
       then responseFile status200 [] path Nothing
       else plain status404 "not-found\n"

blank :: Response
blank = plain status200 ""

plain :: Status -> LBS.ByteString -> Response
plain code = responseLBS code []

sync :: Env -> IO ()
sync Env{..} =
    Index.latest (spec appOptions) appStore
        >>= Index.generate (optTemp appOptions) (optWWW appOptions)

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
    Log.debug l . msg $ " \""
        +++ requestMethod rq
        +++ ' '
        +++ rawPathInfo rq
        +++ rawQueryString rq
        +++ ' '
        +++ show (httpVersion rq)
        +++ "\" "
        +++ statusCode (responseStatus rs)
        +++ " \""
        +++ fromMaybe mempty (lookup "user-agent" $ requestHeaders rq)
        +++ "\" \""
        +++ fromMaybe mempty (lookup "referer" $ requestHeaders rq)
        +++ '"'
    return rs

spec Options{..} = mkInRelease "origin" "label" "codename" "description"
