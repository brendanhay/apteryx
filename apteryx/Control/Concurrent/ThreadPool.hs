-- Module      : Control.Concurrent.ThreadPool
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Control.Concurrent.ThreadPool where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

parForM :: Int          -- ^ Number of workers.
        -> [a]          -- ^ Work items.
        -> (a -> IO b)  -- ^ Work action.
        -> (b -> IO ()) -- ^ Result action.
        -> IO ()        -- ^ Barrier action.
parForM n xs f g = do
    (out, b) <- parMapM n xs f
    a        <- async (go out)
    link a
    (b >> wait a)
        `onException` cancel a
  where
    go out = do
        mx <- atomically $ readTBQueue out
        maybe (return ())
              (\x -> g x >> go out)
              mx

parMapM :: Int                           -- ^ Number of workers.
        -> [a]                           -- ^ Work items.
        -> (a -> IO b)                   -- ^ Work action.
        -> IO (TBQueue (Maybe b), IO ()) -- ^ Result stream, Barrier action.
parMapM n xs f = do
    inp <- atomically newTQueue
    out <- atomically $ newTBQueue n

    ws  <- mapM (const . async $ go inp out) [1..n]

    mapM_ link ws

    mapM_ (atomically . writeTQueue inp . Just) xs
    mapM_ (const . atomically $ writeTQueue inp Nothing) [1..n]

    return (out, mapM_ wait ws >> atomically (writeTBQueue out Nothing))
        `onException` mapM_ cancel ws
  where
    go inp out = do
        mx <- atomically $ readTQueue inp
        maybe (return ())
              (\x -> f x >>= atomically . writeTBQueue out . Just >> go inp out)
              mx
