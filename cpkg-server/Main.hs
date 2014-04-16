{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
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

import           Control.Error
import           Control.Monad
import           Control.Monad.Catch         (finally)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.String
import           Filesystem.Path.CurrentOS   hiding (empty)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error)
import           Network.Wai.Routing         hiding (options)
import           Prelude                     hiding (FilePath, head)
import           System.APT.IO
import           System.APT.Package
import           System.APT.Server.App
import           System.APT.Server.Options
import           System.APT.Types
import qualified System.Logger               as Log
import           System.LoggerT              (msg, (+++), field)
import qualified System.LoggerT              as LogT

default (ByteString)

data Options = Options
    { optHost     :: !String
    , optPort     :: !Word16
    , optKey      :: !Key
    , optIncoming :: !FilePath
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq)

options :: Parser Options
options = Options
    <$> strOption
         ( long "host"
        <> short 'h'
        <> metavar "HOST"
        <> help "Hostname or address to bind on."
        <> value "0.0.0.0"
         )

    <*> option
         ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Port number to listen on. default: 8080"
        <> value 8080
         )

    <*> keyOption
         ( long "key"
        <> short 'k'
        <> metavar "BUCKET/PREFIX"
        <> help "S3 bucket to store packages in."
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Number versions to allow in the package index. default: 3"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

data Env = Env
    { appOptions :: !Options
    , appLogger  :: !Logger
    }

newtype App a = App (ReaderT Env IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadReader Env
             )

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

main :: IO ()
main = parseOptions options >>= serve

serve :: Options -> IO ()
serve o@Options{..} = do
    e <- newEnv o
    runSettings (settings e) (pipeline e) `finally` closeEnv e
  where
    pipeline e = GZip.gzip GZip.def (handler e)
    handler  e = runHandler e . route (prepare $ routes o)
    settings e =
          setHost (fromString optHost)
        . setPort (fromIntegral optPort)
        . setBeforeMainLoop (logStart $ appLogger e)
        . setOnException (logException $ appLogger e)
        . setOnExceptionResponse (const serverError)
        . setTimeout 60
        $ defaultSettings

    logStart l = Log.info l . msg $
        "Listening on " +++ optHost +++ ':' +++ optPort

    logException l _ x = Log.err l $ msg (BS.pack $ show x)

    serverError = setStatus status500 (plain "server-error")

routes :: Options -> Routes a (EitherT Error App) ()
routes Options{..} = do
    renderer (return . LBS.fromStrict . fromMaybe "N/A" . message)

    get  "/i/status" (const $ return empty) true
    head "/i/status" (const $ return empty) true

    get  "/packages" packages request
    post "/packages" (upload optIncoming) $
        accept "application" "x-deb" .&. request

packages :: Request -> Handler
packages _ = return $ responseLBS status200 [] ""

upload :: FilePath -> Media "application" "x-deb" ::: Request -> Handler
upload tmp (_ ::: rq) = do
    c@Control{..} <- receive tmp (requestBody rq)
    LogT.debug $ field "uploaded" (show c)

    put to s3

    return . addHeader "Location" ctlPackage
           $ responseLBS status201 [] (LBS.pack $ show c)

-- FIXME: expose runEnv in amazonka and related auth/credentials
newEnv :: Options -> IO Env
newEnv opts = Env opts <$> Log.new Log.defSettings

closeEnv :: Env -> IO ()
closeEnv = Log.close . appLogger

runApp :: Env -> App a -> IO a
runApp e (App a) = runReaderT a e

runHandler :: Env -> Handler -> IO Response
runHandler e h = runApp e (eitherT onError return h)

onError :: Error -> App Response
onError e = case statusCode code of
    c | c >= 500  -> LogT.err errField
      | c >= 400  -> LogT.debug errField
      | otherwise -> return ()
    >> return (setStatus code . plain $ LBS.fromStrict lbl)
  where
    errField = field lbl ("\"" +++ line +++ "\"")

    (code, lbl, line) = case e of
        MissingField m   -> (status400, "invalid-package", encode m)
        InvalidField m   -> (status400, "invalid-package", encode m)
        ShellError _ _ m -> (status500, "invalid-package", m)
        Exception ex     -> (status500, "server-error",    LBS.pack $ show ex)

    encode = LBS.fromStrict . Text.encodeUtf8

empty :: Response
empty = plain ""

plain :: LBS.ByteString -> Response
plain = responseLBS status200 []

-- setStatus :: Status -> Response -> Response
-- setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
-- setStatus s (ResponseSource _ h x)  = ResponseSource s h x
-- setStatus s (ResponseFile _ h f ff) = ResponseFile s h f ff
-- setStatus s (ResponseRaw x r)       = ResponseRaw x (setStatus s r)

-- addHeader :: HeaderName -> ByteString -> Response -> Response
-- addHeader k v (ResponseFile s h f ff) = ResponseFile s ((k, v):h) f ff
-- addHeader k v (ResponseBuilder s h b) = ResponseBuilder s ((k, v):h) b
-- addHeader k v (ResponseSource s h x)  = ResponseSource s ((k, v):h) x
-- addHeader k v (ResponseRaw s r)       = ResponseRaw s (addHeader k v r)
