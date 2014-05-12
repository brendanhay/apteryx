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
    (
    -- * S3 Sync
      sync

    -- * HTTP Triggers
    , rebuild
    , reindex
    ) where

import           Control.Applicative
import           Control.Exception         (throwIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as BS
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.Zlib         as Conduit
import qualified Data.Foldable             as Fold
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Monoid               hiding (All)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Time
import           Network.HTTP.Conduit      hiding (path)
import           Prelude                   hiding (lookup)
import           System.APT.IO
import qualified System.APT.Package        as Pkg
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Directory
import           System.Exit
import           System.IO
import           System.IO.Temp
import           System.Logger.Message     ((+++))
import           System.Process

default (Builder)

sync :: FilePath
     -> FilePath
     -> [Arch]
     -> (UTCTime -> Map Arch (Set Package) -> InRelease)
     -> Store
     -> IO ()
sync tmp dest as ctor s = do
    c <- latest ctor s >>= generate tmp dest as
    case c of
        ExitSuccess   -> return ()
        ExitFailure _ -> throwIO $
            shellError ("Failed to copy " ++ tmp ++ " to " ++ dest)

latest :: (UTCTime -> Map Arch (Set Package) -> InRelease)
       -> Store
       -> IO InRelease
latest ctor s = ctor <$> getCurrentTime <*> (Store.entries s >>= par)
  where
    par = fmap (Map.unionsWith (<>)) . parMapM (Fold.foldrM metadata mempty)

    metadata o m = do
        r <- Store.metadata o s
        case r of
            Left  e -> throwIO e
            Right p -> return $ Map.insertWith (<>) (entArch p) (Set.singleton p) m

generate :: FilePath -> FilePath -> [Arch] -> InRelease -> IO ExitCode
generate tmp dest as r@InRelease{..} =
    withTempDirectory tmp "apt." $ \path -> do
        let pkgs  = Map.union relPkgs $ Map.fromList (zip as (repeat mempty))
            archs = fromMaybe mempty (Map.lookup All pkgs)

            ps  | All `elem` as = pkgs
                | otherwise     = Map.delete All pkgs

            f k | k `elem` as = (<> archs)
                | otherwise   = id

        ids <- parMapM (release path) (Map.toList $ Map.mapWithKey f ps)

        writef (path ++ "/Release") $ \hd -> do
            putBuilders hd (Pkg.toIndex r)
            putBuilders hd (Pkg.toIndex $ concat ids)

        createDirectoryIfMissing True dest

        diff path dest >>= mapM_ removePath

        system $ "cp -rf " ++ path ++ "/* " ++ dest ++ "/"
  where
    release base (arch, ps) = liftIO $ do
        let dir  = base ++ BS.unpack ("/binary-" <> toByteString arch)
            rel  = dir ++ "/Release"
            pkg  = dir ++ "/Packages"
            pkgz = dir ++ "/Packages.gz"

        createDirectoryIfMissing True dir

        writef rel $ \hd ->
            putBuilders hd . Pkg.toIndex $ Release relCode relOrigin relLabel arch

        writef pkg $ \hd ->
            putBuilders hd . concatMap contents $ Set.toList ps

        readf pkg $ \x ->
            writef pkgz $ \y ->
                Conduit.sourceHandle x $= Conduit.gzip $$ Conduit.sinkHandle y

        forM [rel, pkg, pkgz] $ \x ->
            Index (drop (length base + 1) x) <$> getFileStat x

    writef p f = withFile p WriteMode $ \hd -> do
        hSetBinaryMode hd True
        hSetBuffering hd (BlockBuffering Nothing)
        f hd

    readf p = withFile p ReadMode

    contents p@Entry{..} =
        let f = "Filename: packages/"
             +++ entArch
             +++ "/"
             +++ entName
             +++ "/"
             +++ entVers
         in Pkg.toIndex p ++ [f, "\n"]

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

putBuilders :: Handle -> [Builder] -> IO ()
putBuilders hd = mapM_ (hPutBuilder hd . (<> "\n"))
