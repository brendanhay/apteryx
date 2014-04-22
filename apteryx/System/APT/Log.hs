{-# LANGUAGE OverloadedStrings #-}

-- Module      : System.APT.Log
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Log
    (a Logger
    , newLogger

    , err_
    , say_
    , say
    , sayT_
    , sayT
    ) where

import           Control.Monad.IO.Class
import           Data.ByteString         (ByteString)
import           Data.Text.Format        (Format)
import qualified Data.Text.Format        as Text
import           Data.Text.Format.Params (Params)
import           System.Logger
import           System.LoggerT          (MonadLogger)
import qualified System.LoggerT          as LogT

newLogger :: IO Logger
newLogger = new defSettings

err_ :: (MonadIO m, ToBytes a) => Logger -> ByteString -> a -> m ()
err_ lgr lbl = err lgr . field lbl

say_ :: (MonadIO m, ToBytes a) => Logger -> ByteString -> a -> m ()
say_ lgr lbl = debug lgr . field lbl

say :: (MonadIO m, Params ps) => Logger -> ByteString -> Format -> ps -> m ()
say lgr lbl fmt ps = debug lgr . field lbl $ Text.format fmt ps

sayT_ :: (MonadLogger m, ToBytes a) => ByteString -> a -> m ()
sayT_ lbl = LogT.debug . field lbl

sayT :: (MonadLogger m, Params ps) => ByteString -> Format -> ps -> m ()
sayT lbl fmt ps = LogT.debug . field lbl $ Text.format fmt ps
