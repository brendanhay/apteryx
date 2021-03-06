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
    ( Logger
    , newLogger
    , getLogger

    , err_
    , say_
    , say
    , sayT_
    , sayT

    , build

    , Text.Shown (..)
    , Text.Only  (..)
    ) where

import           Control.Monad.IO.Class
import           Data.Text.Buildable
import           Data.Text.Format        (Format)
import qualified Data.Text.Format        as Text
import           Data.Text.Format.Params (Params)
import           System.APT.Types
import qualified System.IO.Unsafe        as Unsafe
import           System.Logger
import           System.Logger.Class     (MonadLogger)
import qualified System.Logger.Class     as Log

newLogger :: IO Logger
newLogger = new $ defSettings { bufSize = 0 }

getLogger :: Logger
getLogger = Unsafe.unsafePerformIO newLogger
{-# NOINLINE getLogger #-}

err_ :: (MonadIO m, ToBytes a, ToBytes b) => a -> b -> m ()
err_ lbl = err getLogger . field (toByteString lbl)

say_ :: (MonadIO m, ToBytes a, ToBytes b) => a -> b -> m ()
say_ lbl = info getLogger . field (toByteString lbl)

say :: (MonadIO m, ToBytes a, Params ps) => a -> Format -> ps -> m ()
say lbl fmt ps = info getLogger . field (toByteString lbl) $ Text.format fmt ps

sayT_ :: (MonadLogger m, ToBytes a, ToBytes b) => a -> b -> m ()
sayT_ lbl = Log.info . field (toByteString lbl)

sayT :: (MonadLogger m, ToBytes a, Params ps) => a -> Format -> ps -> m ()
sayT lbl fmt ps = Log.info . field (toByteString lbl) $ Text.format fmt ps
