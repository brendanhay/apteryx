-- Module      : S3Apt.Server
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.Server where

import qualified System.Logger               as Logger

run :: Opts -> IO ()
run o = do
  --   e <- mkEnv <$> pure m       -- 
  --              <*> pure o
  --              <*> new defSettings { output = StdOut, format = "" }
  --              <*> D.empty 128
  --              <*> (toASCIIBytes <$> nextRandom)
  --              <*> newManager defaultManagerSettings { managerConnCount = 128 }
  --              <*> createSystemRandom
  --   k <- newKey
  --   let application = route (requestCounter m (prepare (sitemap k)))
  --       checked     = validate k (Z.options p ["/i/*", "/i/push/*/*"])
  --       start       = Gzip.gzip Gzip.def $ \r -> runCannon e (checked application r) r
  --   Logger.info (applog e) $
  --       ("ID" =: ident e) . msg ("Listening on " +++ host o +++ ':' +++ port o)
  --   runSettings (settings e) start `finally` Logger.close (applog e)
  -- where
  --   settings e =
  --         setHost (fromString $ host (opts e))
  --       . setPort (fromIntegral . port . opts $ e)
  --       . setOnException (logException (applog e))
  --       . setOnExceptionResponse onInternalError
  --       . setTimeout (fromIntegral $ 25 # Minute)
  --       $ defaultSettings

  --   logException lgr r x =
  --       Logger.err lgr $ maybe id requestIdMsg r . msg (pack (show x))

  --   onInternalError = errorRs status500 "server-error" . Text.pack . show

-- sitemap :: Routes a Cannon ()
-- sitemap = do
--     renderer jsonFormat

--     get "/await" await $
--         fromVault k .&. request

--     post "/i/push/:user/:conn" push $
--         capture "user" .&. capture "conn" .&. request

--     get "/i/monitoring" monitoring $
--         accept "application" "json"

--     get  "/i/status" (const $ return empty) true

--     head "/i/status" (const $ return empty) true

--   where
--     jsonFormat e = return . encode $
--         Error (status e)
--               "client-error"
--               (maybe "N/A" (decodeUtf8 . fromStrict) (message e))

-- push :: ByteString ::: ByteString ::: Request -> Cannon Response
-- push (user ::: conn ::: req) = do
--     let k = mkKey user conn
--     d <- clients
--     debug $ client (key2bytes k) . msg "push"
--     c <- D.lookup k d
--     case c of
--         Nothing -> do
--             warn $ client (key2bytes k) . msg "push: not found"
--             return clientGone
--         Just x  -> do
--             b <- readBody req
--             e <- wsenv
--             runWS e $
--                 sendMsg b k x >> return empty
--                 `catchAll`
--                 const (terminate k x >> return clientGone)
--   where
--     clientGone = errorRs status410 "general" "client gone"

-- await :: Token Access ::: Request -> Cannon Response
-- await (a ::: r) = do
--     l <- logger
--     e <- wsenv
--     case websocketsApp defaultConnectionOptions (wsapp a l e) r of
--         Nothing -> return $ errorRs status426 "request-error" "websocket upgrade required"
--         Just rs -> return rs
--   where
--     status426 = mkStatus 426 "Upgrade Required"

