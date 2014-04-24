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
module System.APT.Index
    ( Index
    , new

    , path
    , member
    , trySync
    , syncIfMissing
    , sync

    , rebuild
    , reindex
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.ThreadPool
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as BS
import qualified Data.Foldable             as Fold
import           Data.Maybe
import           Data.Monoid
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as Path
import           Network.HTTP.Conduit      hiding (path)
import qualified Network.HTTP.Types        as HTTP
import           System.APT.IO
import qualified System.APT.Package        as Pkg
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Directory
import           System.IO

data Index = Index
    { _n     :: !Int
    , _path  :: !Path
    , _temp  :: !Path
    , _store :: Store
    , _lock  :: MVar ()
    }

new :: MonadIO m => Int -> Path -> Path -> Store -> m Index
new n dir tmp s = liftIO $
    ensureExists dir >>
        Index n (dir </> "Packages") tmp s <$> newMVar ()

path :: Index -> Path
path = _path

-- | Check if a specific arch/name/vers exists in the package index.
member :: MonadIO m => Arch -> Name -> Vers -> Index -> m Bool
member a n v i = undefined

-- | Regenerate the package index and write it to disk,
--   ensuring only one active sync is in progress.
trySync :: MonadIO m => Index -> m Bool
trySync i@Index{..} = liftIO $ do
    m <- tryTakeMVar _lock
    maybe (return False)
          (\v -> do
              void $ unsafeSync i `forkFinally` const (putMVar _lock v)
              return True)
          m

-- | Regenerate the package index if it doesn't exist on disk.
syncIfMissing :: MonadIO m => Index -> m ()
syncIfMissing i@Index{..} = liftIO $ do
    let f = Path.encodeString _path
    x <- doesFileExist f
    unless x $
        bracket (takeMVar _lock)
                (putMVar _lock)
                (const $ do
                    y <- doesFileExist f
                    unless y $ unsafeSync i)

-- | Regenerate the package index and write it to disk.
sync :: MonadIO m => Index -> m ()
sync i@Index{..} = liftIO $
    bracket (takeMVar _lock)
            (putMVar _lock)
            (const $ unsafeSync i)

-- | Synchronously regenerate the index, without locking.
unsafeSync :: Index -> IO ()
unsafeSync Index{..} =
    withTempFile _temp ".packages" $ \src hd -> do
        hSetBinaryMode hd True
        hSetBuffering hd (BlockBuffering Nothing)

        let get = mapM (\x -> Store.metadata (entKey x) _store)
            put = hPutBuilder hd
                . Fold.foldMap Pkg.toBuilder
                . reverse
                . catMaybes

        Store.entries _store >>=
            parForM _n get put

        hClose hd >> copyFile src (Path.encodeString _path)

-- FIXME: Move to a more relevant location

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
