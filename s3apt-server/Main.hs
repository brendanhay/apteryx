{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main (main) where

import           Control.Applicative         hiding (empty)
import           Control.Error
import           Control.Monad
import           Control.Monad.Catch         (finally)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Control.Monad.Trans.Either
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Conduit
import qualified Data.Conduit.Binary         as Conduit
import           Data.Int
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import qualified Data.Text.Lazy              as LText
import qualified Data.Text.Lazy.Encoding     as LText
import           Filesystem.Path.CurrentOS   hiding (empty)
import           Network.HTTP.Client         (RequestBody (..), Cookie)
import           Network.HTTP.Types
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Internal
import qualified Network.Wai.Middleware.Gzip as GZip
import           Network.Wai.Predicate       hiding (Error)
import           Network.Wai.Routing         hiding (options)
import           Prelude                     hiding (FilePath, head)
import           S3Apt.IO
import           S3Apt.Server.App
import           S3Apt.Server.IO
import           S3Apt.Server.Options
import           S3Apt.Server.Package
import           S3Apt.Server.Types
import           System.IO                   (hClose)
import qualified System.Logger               as Log
import           System.LoggerT              (msg, (+++), field)
import qualified System.LoggerT              as LogT

default (ByteString)

main :: IO ()
main = do
    o <- parseOptions options
    e <- newEnv o
    runSettings (settings o e) (pipeline o e) `finally` closeEnv e
  where
    pipeline o e = GZip.gzip GZip.def (handler o e)
    handler  o e = runHandler e . route (prepare $ sitemap o)
    settings o e =
          setHost (fromString $ optHost o)
        . setPort (fromIntegral $ optPort o)
        . setBeforeMainLoop (logStart o $ envLogger e)
        . setOnException (logException $ envLogger e)
        . setOnExceptionResponse (const $ serverError)
        . setTimeout 60
        $ defaultSettings

    logStart o l = Log.info l . msg $
        "Listening on " +++ optHost o +++ ':' +++ optPort o

    logException l _ x = Log.err l $ msg (BS.pack $ show x)

    serverError = setStatus status500 (plain "server-error")

sitemap :: Options -> Routes a (EitherT Error App) ()
sitemap Options{..} = do
    renderer predicateError

    get  "/i/status" (const $ return empty) true
    head "/i/status" (const $ return empty) true

    get  "/i/packages" packages request

    post "/packages" (upload optIncoming) $
            header "Content-Length"
        .&. request
  where
    predicateError e = return . LBS.pack . show $
        Error (status e)
              "client-error"
              (maybe "N/A" Text.decodeUtf8 (message e))

packages :: Request -> Handler
packages rq = return $ responseLBS status200 [] ""

upload :: FilePath -> Int64 ::: Request -> Handler
upload dir (size ::: rq) = handle (withTempFile dir ".deb" receive) >>= respond
  where
    receive path hd = do
        scriptIO $ (requestBody rq $$ Conduit.sinkHandle hd) >> hClose hd
        packageControl path

    handle = runEitherT . mapEitherT liftIO
--        . fmapLT (Error status500 "server-error" . Text.pack)

    respond (Left Error{..}) = return
        $ responseLBS errStatus [] (LText.encodeUtf8 $ LText.fromStrict errMessage)
    respond (Right c@Control{..}) = return
        . addHeader "Location" ctlPackage
        $ responseLBS status201 [] (LBS.pack $ show c)

    -- packageTooLarge = Error status413 "client-error" "Package too large."
    -- invalidLength = Error status400 "invalid-length" "Invalid content length."
    -- duplicatePackage      = Error status409 "client-error" "Duplicate package."

type Handler = EitherT Error App Response

runHandler :: Env -> Handler -> IO Response
runHandler e h = runApp e (eitherT onError return h)

data Error = Error
    { errStatus  :: !Status
    , errLabel   :: !Text
    , errMessage :: !Text
    } deriving (Show)

onError :: Error -> App Response
onError e@(Error c l m) = case statusCode c of
    x | x >= 500  -> LogT.err   errField
      | x >= 400  -> LogT.debug errField
      | otherwise -> return ()
    >> return (setStatus c (plain "on-error"))
  where
    errField = field (Text.encodeUtf8 l) ("\"" +++ m +++ "\"")

-- plain :: Lazy.ByteString -> Response
plain = responseLBS status200 []

empty = plain ""

setStatus :: Status -> Response -> Response
setStatus s (ResponseBuilder _ h b) = ResponseBuilder s h b
setStatus s (ResponseSource _ h x)  = ResponseSource s h x
setStatus s (ResponseFile _ h f ff) = ResponseFile s h f ff
setStatus s (ResponseRaw x r)       = ResponseRaw x (setStatus s r)

addHeader :: HeaderName -> ByteString -> Response -> Response
addHeader k v (ResponseFile s h f ff) = ResponseFile s ((k, v):h) f ff
addHeader k v (ResponseBuilder s h b) = ResponseBuilder s ((k, v):h) b
addHeader k v (ResponseSource s h x)  = ResponseSource s ((k, v):h) x
addHeader k v (ResponseRaw s r)       = ResponseRaw s (addHeader k v r)
