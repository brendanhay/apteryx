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
import           S3Apt.IO
import           S3Apt.Package
import           S3Apt.Server.App
import           S3Apt.Server.Options
import           S3Apt.Types
import qualified System.Logger               as Log
import           System.LoggerT              (msg, (+++), field)
import qualified System.LoggerT              as LogT

default (ByteString)

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
