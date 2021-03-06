{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
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
import           Control.Error               hiding (err)
import           Control.Exception           (throwIO)
import           Control.Monad
import           Control.Monad.Catch         hiding (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import           Data.Word
import           Network.AWS
import qualified Network.HTTP.Conduit        as HTTP
import           Network.HTTP.ReverseProxy
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error, err, hd)
import           Network.Wai.Routing         hiding (options)
import           Options.Applicative         hiding (header)
import           Prelude                     hiding (concatMap, head, log)
import           System.APT.Compression
import           System.APT.IO
import qualified System.APT.Index            as Index
import           System.APT.Log
import           System.APT.Options
import           System.APT.Store            (Store)
import qualified System.APT.Store            as Store
import           System.APT.Types
import           System.FilePath
import qualified System.Logger               as Log
import           System.Logger.Class         hiding (Error)

default (Builder)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Bucket
    , optTemp     :: !FilePath
    , optWWW      :: !FilePath
    , optVersions :: !Int
    , optArchs    :: [Arch]
    , optCode     :: !Text
    , optDesc     :: !Text
    , optDebug    :: !Bool
    } deriving (Eq)

-- architectures:
--  remove all from InRelease
--  if specified, will create releases for each one containing the 'all' packages
--  if necessary
--  merge all into others

-- distributions:
--  will generate a release for each distribution

-- archive:
--  add to release indicies, should be same as the distribution name

-- translations:
--  /dists/<dist>/i18n/Translation-en.<ext>
    -- Package
    -- Description-md5
    -- Description-$LANG

options :: Parser Options
options = Options
    <$> strOption
         ( long "host"
        <> short 'H'
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

    <*> some (fromOption
         ( long "arch"
        <> short 'a'
        <> metavar "ARCH"
        <> help "Architectures to union 'all' with. [required]"
         ))

    <*> textOption
         ( long "codename"
        <> short 'c'
        <> metavar "STR"
        <> help "Codename of the repository the server will respond to. [required]"
         )

    <*> textOption
         ( long "description"
        <> short 'd'
        <> metavar "STR"
        <> help "Description of the repository for each Release file. [default]"
        <> value "A Magical APT Server"
         )

    <*> switch
         ( long "debug"
        <> help "Print debug output."
         )

data Env = Env
    { _options :: !Options
    , _aws     :: !AWSEnv
    , _logger  :: !Logger
    , _manager :: !HTTP.Manager
    , _lock    :: MVar ()
    }

newEnv :: Options -> IO Env
newEnv o@Options{..} = Env o
    <$> getAWSEnv optDebug
    <*> pure getLogger
    <*> HTTP.newManager HTTP.conduitManagerSettings
    <*> newMVar ()

closeEnv :: Env -> IO ()
closeEnv Env{..} = Log.close _logger >> HTTP.closeManager _manager

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
    log l f = do
        g <- asks _logger
        Log.log g l f

instance MonadLogger (EitherT e App) where
    log l f = lift (log l f)

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
    r <- runApp e . runEitherT $ sync (const id)
    either throwIO (const $ return ()) r

    say_ "server" "Apteryx server starting..."
    runSettings (settings e) (middleware e) `finally` closeEnv e
  where
    middleware e = logRequest (_logger e) . GZip.gzip GZip.def $ handler e
    handler    e = runHandler e . route (prepare $ routes optCode)

    settings e =
          setHost (fromString optHost)
        . setPort (fromIntegral optPort)
        . setBeforeMainLoop (logStart $ _logger e)
        . setOnException (logException $ _logger e)
        . setOnExceptionResponse (const serverError)
        $ defaultSettings

    logStart l =
        Log.info l $ msg ("Listening on " +++ optHost +++ ':' +++ optPort)

    logException l _ = Log.err l . msg . show

    serverError = plain status500 "server-error\n"

routes :: Text -> Routes a (EitherT Error App) ()
routes c = do
    get   "/i/status" (const $ return blank) true
    head  "/i/status" (const $ return blank) true

    post  "/packages" (const triggerRebuild) true

    get   "/packages/:arch/:name/:vers" getPackage
         $  capture "arch"
        .&. capture "name"
        .&. capture "vers"
        .&. request

    patch "/packages/:arch/:name/:vers" addPackage
         $  capture "arch"
        .&. capture "name"
        .&. capture "vers"

    arch "Release"
    arch "Packages"
    arch ("Packages" <.> compressExt)

    dist "Release"
    dist ("i18n/Translation-en" <.> compressExt)
  where
    arch x = get ("/dists/" <> code <> "/s3/:arch/" <> fromString x) (getArch x)
         $  capture "arch"
        .&. opt (header "Range")
        .&. opt (header "If-Range")
        .&. opt (header "If-Modified-Since")

    dist x = get ("/dists/" <> code <> "/" <> fromString x) (getIndex x)
         $  opt (header "Range")
        .&. opt (header "If-Range")
        .&. opt (header "If-Modified-Since")

    code = Text.encodeUtf8 c

getPackage :: Arch ::: Name ::: Vers ::: Request -> Handler
getPackage (a ::: n ::: v ::: rq) = do
    e@Env{..} <- ask

    u <- runStore e (Store.presign (optKey _options) (Entry n v a ()) 10)
        >>= either throwM return

    let (host, url) = BS.break (== '/') (BS.drop 8 u)
        (path, qry) = BS.break (== '?') url

    let f = WPRModifiedRequest
              (defaultRequest
                   { requestMethod  = "GET"
                   , rawPathInfo    = path
                   , requestHeaders = []
                   , isSecure       = True
                   , rawQueryString = BS.drop 1 qry
                   })
              (ProxyDest host 443)

    sayT "proxy" "{}" [BS.unpack $ host <> path <> qry]

    liftIO $ waiProxyTo (return . const f) defaultOnExc _manager rq

addPackage :: Arch ::: Name ::: Vers -> Handler
addPackage (a ::: n ::: v) = do
    e@Env{..} <- ask
    r         <- runStore e (Store.metadata (optKey _options) x)
    either (const missing)
           (const found)
           r
  where
    missing = do
        s "Unable to find package {}" [x]
        return $ plain status404 "not-found\n"

    found = do
        s "Found package {}, rebuilding local index" [x]
        sync $ \l -> withMVar l . const
        return $ plain status200 "index-rebuilt\n"

    x = Entry n v a ()
    s = sayT "add-package"

triggerRebuild :: Handler
triggerRebuild = sync lock >>= respond
  where
    respond True = do
        sayT_ n "Triggering local index rebuild"
        return $ plain status202 "triggered-rebuild\n"

    respond False = do
        sayT_ n "Rebuild already in progress"
        return $ plain status200 "rebuild-in-progress\n"

    n = "rebuild"

    lock l f = do
        m <- tryTakeMVar l
        maybe (return False)
              (\v -> do
                  void $ f `forkFinally` const (putMVar l v)
                  return True)
              m

getArch :: FilePath
        -> Arch ::: Maybe Range ::: Maybe Time ::: Maybe Time
        -> Handler
getArch p (a ::: r ::: f ::: m) =
    getIndex ("binary-" ++ show a </> p) (r ::: f ::: m)

getIndex :: FilePath
         -> Maybe Range ::: Maybe Time ::: Maybe Time
         -> Handler
getIndex p (range ::: ifRange ::: ifModified) = do
    path <- asks ((</> p) . optWWW . _options)
    st   <- catchError (getFileStat path)

    return $ case st of
        Nothing       -> plain status404 "not-found\n"
        Just (sz, ts) ->
            -- If-Modified-Since allows a 304 Not Modified to be returned
            -- if content is unchanged.
            case ifModified of
                Just x ->
                    if ts > x
                        then responseFile status200 hs path Nothing
                        else plain status304 "not-modified\n"
                Nothing ->
                    -- If the representation is unchanged, send me the part(s)
                    -- that I am requesting in Range; otherwise, send me the
                    -- entire representation.
                    case ifRange of
                        Just x ->
                            if ts <= x
                                then case range of
                                    Just r -> case rangeToFilePart sz r of
                                        Just fp ->
                                            responseFile status206 hs path (Just fp)
                                        Nothing ->
                                            plain status416 "invalid-range\n"
                                    Nothing ->
                                        responseFile status200 hs path Nothing
                                else responseFile status200 hs path Nothing
                        Nothing ->
                            responseFile status200 hs path Nothing
  where
    hs = [ ("Cache-Control", "no-cache, no-store, must-revalidate")
         , ("Pragma",        "no-cache")
         , ("Expires",       "0")
         ]

sync :: (MVar () -> IO () -> IO a) -> EitherT Error App a
sync f = do
    e@Env{..} <- ask

    let Options{..} = _options
        ctor        = mkInRelease optCode optDesc
        display     = Log.debug _logger . msg . show

    catchError . f _lock $
        runStore e (Index.sync optKey optTemp optWWW optArchs ctor) >>=
            either display return

runStore :: MonadIO m => Env -> Store a -> m (Either Error a)
runStore Env{..} s = liftIO $ fmapL (awsError . show) <$>
    Store.run (optVersions _options) _aws s

blank :: Response
blank = plain status200 ""

plain :: Status -> LBS.ByteString -> Response
plain code = responseLBS code []

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
    Log.debug l . msg $ '"'
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

rangeToFilePart :: Size -> Range -> Maybe FilePart
rangeToFilePart sz (Range f g)
    | o  > n    = Nothing
    | bs > n    = Nothing
    | o  > bs   = Nothing
    | otherwise = Just (FilePart o bs n)
  where
    o  = f sz
    bs = g sz
    n  = fromIntegral (unSize sz)
