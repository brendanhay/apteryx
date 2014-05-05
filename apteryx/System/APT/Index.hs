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

module System.APT.Index
    ( latest
    , generate

    , rebuild
    , reindex
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Par.Combinator
import           Control.Monad.Par.IO
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as BS
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.Zlib         as Conduit
import qualified Data.Foldable             as Fold
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Time
import           Network.HTTP.Conduit      hiding (path)
import           Prelude                   hiding (lookup)
import           System.APT.IO
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Directory
import           System.IO
import           System.IO.Temp
import           System.Logger.Message     ((+++))
import           System.Process

default (Builder)

latest :: (UTCTime -> Map Arch (Set Package) -> InRelease)
       -> Store
       -> IO InRelease
latest ctor s = ctor
    <$> getCurrentTime
    <*> (Store.entries s >>= parallel)
  where
    parallel = fmap (Map.unionsWith (<>))
        . runParIO
        . parMapM (Fold.foldrM metadata mempty)

    metadata e m =
        maybe m (\p -> Map.insertWith (<>) (entArch p) (Set.singleton p) m)
            <$> Store.metadata e s

generate :: FilePath -> FilePath -> InRelease -> IO ()
generate tmp dest r@InRelease{..} =
    withTempDirectory tmp "apt." $ \path -> do
        ids <- runParIO $ parMapM (release path) (Map.toList relPkgs)

        let rel = path ++ "/Release"
            inr = path ++ "/InRelease"

        writef rel $ \hd -> do
            hPutBuilder hd (bytes r)
            hPutBuilder hd (bytes $ concat ids)

        copyFile rel inr

        createDirectoryIfMissing True dest

        diff path dest >>= mapM_ removePath

        -- FIXME: in haskell
        void $ rawSystem "cp" ["-rf", path ++ "/", dest ++ "/"]
  where
    release base (arch, ps) = liftIO $ do
        let dir  = base ++ BS.unpack ("/binary-" <> toByteString arch)
            rel  = dir ++ "/Release"
            pkg  = dir ++ "/Packages"
            pkgz = dir ++ "/Packages.gz"

        createDirectoryIfMissing True dir

        writef rel $ \hd ->
            hPutBuilder hd (bytes $ Release relOrigin relLabel arch)

        writef pkg $ \hd ->
            mapM_ (hPutBuilder hd . contents) (Set.toList ps)

        readf pkg $ \x ->
            writef pkgz $ \y ->
                Conduit.sourceHandle x $= Conduit.gzip $$ Conduit.sinkHandle y

        forM [rel, pkg, pkgz] $ \x ->
            Index (drop (length base + 1) x) <$> getFileStat x

    contents p@Entry{..} =
        let name = entArch +++ "/" +++ entName +++ "/" +++ entVers
         in bytes p <> ("Filename: packages/" +++ name) <> "\n\n"

    writef p f = withFile p WriteMode $ \hd -> do
        hSetBinaryMode hd True
        hSetBuffering hd (BlockBuffering Nothing)
        f hd

    readf p = withFile p ReadMode

-- FIXME: Move to a more relevant location

-- | Trigger a remote rebuild of the index.
rebuild :: MonadIO m => String -> m ()
rebuild host = liftIO $ do
    rq <- parseUrl (host <> "/packages")
    void . withManager $ httpLbs (rq { method = "POST" })

-- | Trigger a remote reindex of a specified package.
reindex :: MonadIO m => Package -> String -> m ()
reindex Entry{..} host = liftIO $ do
    rq <- parseUrl (host <> url)
    void . withManager $ httpLbs (rq { method = "PATCH" })
  where
    url = BS.unpack $ mconcat
        [ "/packages/"
        , toByteString entArch
        , "/"
        , toByteString entName
        , "/"
        , toByteString (urlEncode entVers)
        ]
