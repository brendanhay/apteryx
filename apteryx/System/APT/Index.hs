{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Conduit
import           Data.IORef
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import qualified Filesystem.Path.CurrentOS as Path
import           Network.HTTP.Conduit
import qualified Network.HTTP.Types        as HTTP
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Logger.Message     ((+++))

default (ByteString)

type Key = (Arch, Name)
type Val = Map Vers Package

data Index = Index
    { _store :: Store
    , _ref   :: IORef (Map Key Val)
    }

-- | Create an index from the given store.
new :: MonadIO m => Store -> m Index
new s = Index s `liftM` liftIO (newIORef mempty)

-- | Check if the index is fresher relative to the given time.
--   Used to compare in memory against disk Packages
-- fresher = undefined

-- | Create a serialised version of the index suitable to be streamed to a file.
contents :: MonadIO m => Index -> m (Source IO ByteString)
contents = undefined

member :: MonadIO m => Arch -> Name -> Vers -> Index -> m Bool
member a n v i = liftIO $ maybe False (Map.member v) . Map.lookup (a, n)
    <$> readIORef (_ref i)

-- | Insert a package into the index, replacing any existing description.
insert :: MonadIO m => Arch -> Name -> Vers -> Index -> m (Either Error Package)
insert a n v i = do
    m <- Store.metadata a n v (_store i)
    maybe (msg $ "Unable to find S3 metadata for: " +++ a +++ '/' +++ n +++ '/' +++ v)
          add
          m
  where
    add p@Package{..}
        | a /= pkgArch = msg $ "Supplied Arch vs S3: " +++ pkgArch +++ " /= " +++ a
        | n /= pkgName = msg $ "Supplied Name vs S3: " +++ pkgName +++ " /= " +++ n
        | otherwise    = do
            modify (_ref i) (Map.insertWith (<>) (a, n) (Map.singleton v p))
            return (Right p)

    msg :: (MonadIO m, ToBytes a) => a -> m (Either Error Package)
    msg = return . Left . missingPackage

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

modify :: MonadIO m => IORef (Map k v) -> (Map k v -> Map k v) -> m ()
modify r f = liftIO . atomicModifyIORef' r $ \m -> (f m, ())
