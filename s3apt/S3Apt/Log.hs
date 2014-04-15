{-# LANGUAGE OverloadedStrings #-}

-- Module      : S3Apt.Log
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.Log
    ( say_
    , say
    ) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text               (Text)
import           Data.Text.Buildable
import           Data.Text.Format        (Format)
import qualified Data.Text.Format        as Text
import           Data.Text.Format.Params (Params)
import qualified Data.Text.Lazy.IO       as LText
import qualified System.IO.Unsafe        as Unsafe

say_ :: MonadIO m => Text -> Format -> m ()
say_ lbl f = say lbl f ([] :: [Text])

say :: (MonadIO m, Params ps) => Text -> Format -> ps -> m ()
say lbl f ps = do
    tid <- (drop 9 . show) `liftM` liftIO myThreadId
    liftIO . withMVar lock $
        const (LText.putStrLn $ label tid <> Text.format f ps)
  where
    label tid = Text.format "[{}] [{}] "
        [ Text.right 5  ' ' $ build tid
        , Text.right 40 ' ' $ build lbl
        ]

lock :: MVar ()
lock = Unsafe.unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}
