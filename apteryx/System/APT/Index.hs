{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : System.APT.Index
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Interface to the in-memory index of package metadata.
module System.APT.Index where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as BS
import           Data.Conduit
import           Data.Conduit
import qualified Data.Conduit.List as Conduit
import           Data.IORef
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Filesystem.Path.CurrentOS as Path
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types        as HTTP
import qualified System.APT.Package        as Pkg
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Logger.Message     ((+++))

-- | Trigger a remote rebuild of the index.
rebuild :: MonadIO m => String -> m ()
rebuild host = liftIO $ do
    rq <- parseUrl addr
    _  <- withManager $ httpLbs (rq { method = "POST" })
    return ()
  where
    addr = host <> "/packages"

-- | Trigger a remote reindex of a specified package.
reindex :: MonadIO m => Package -> String -> m ()
reindex Package{..} host = liftIO $ do
    rq <- parseUrl addr
    _  <- withManager $ httpLbs (rq { method = "PATCH" })
    return ()
  where
    addr = host <> url

    url = BS.unpack $ mconcat
        [ "/packages/"
        , toByteString pkgArch
        , "/"
        , toByteString pkgName
        , "/"
        , HTTP.urlEncode True (toByteString pkgVersion)
        ]
