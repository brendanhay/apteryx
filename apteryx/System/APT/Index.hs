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

import Network.AWS (AWSError)
import           Control.Applicative
import           Control.Arrow                    (first, second)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader       (ask)
import           Control.Monad.Trans.Resource     (runResourceT)
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8            as BS
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import qualified Data.Conduit.List                as Conduit
import qualified Data.Foldable                    as Fold
import           Data.List                        (intersperse, sort)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import           Data.Monoid                      hiding (All)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Time
import           Network.HTTP.Conduit             hiding (path)
import           Prelude                          hiding (lookup)
import           System.APT.Compression
import           System.APT.IO
import qualified System.APT.Package               as Pkg
import           System.APT.Store                 (Store)
import qualified System.APT.Store                 as Store
import           System.APT.Types
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.Logger.Message            ((+++))
import           System.Process

default (Builder)

sync :: FilePath
     -> FilePath
     -> [Arch]
     -> (UTCTime -> Map Arch (Set Package) -> InRelease)
     -> Store ()
sync tmp dest as ctor = do
    r <- latest ctor
    c <- liftIO $ generate tmp dest as r
    case c of
        ExitSuccess   -> return ()
        ExitFailure _ -> throwM $
            shellError ("Failed to copy " ++ tmp ++ " to " ++ dest)

latest :: (UTCTime -> Map Arch (Set Package) -> InRelease) -> Store InRelease
latest ctor = do
    e  <- ask
    rs <- Store.entries >>= parMapM (foldM (meta e) mempty)
    t  <- liftIO getCurrentTime
    return $! ctor t (Map.unionsWith (<>) (map snd rs))
  where
    f e acc o = liftIO $ (`g` acc) <$> Store.run' e (Store.metadata o)

    g (Left  l) = first  (l :)
    g (Right x) = second (Map.insertWith (<>) (entArch x) (Set.singleton x))

generate :: FilePath -> FilePath -> [Arch] -> InRelease -> IO ExitCode
generate tmp dest as r@InRelease{..} =
    withTempDirectory tmp "apt." $ \path -> do
        let i18n = path </> "i18n"
            en   = i18n </> "Translation-en" <.> compressExt

        createDirectoryIfMissing True i18n

        -- i18n/Translation-en.<ext>
        writeHandle en $ \hd -> runResourceT $
            Conduit.sourceList packages
                $= Conduit.concatMap (Set.toList . snd)
                $= Conduit.map (joinBuilders . Pkg.toIndex . Translate)
                $= Conduit.map ((<> "\n") . toByteString)
                $= compressConduit
                $$ Conduit.sinkHandle hd

        ids <- (:)
            <$> index path en
            <*> (concat <$> parMapM (release path) packages)

        -- i18n/Release
        writeHandle (path </> "Release") $ \hd -> do
            putBuilders hd (Pkg.toIndex r)
            putBuilders hd (Pkg.toIndex $ sort ids)

        createDirectoryIfMissing True dest

        diff path dest >>= mapM_ removePath

        -- FIXME: Avoid shelling out?
        system $ "cp -rf " ++ path ++ "/* " ++ dest ++ "/"
  where
    packages = Map.toList $ Map.mapWithKey filtered valid
      where
        valid
            | All `elem` as = ps
            | otherwise     = Map.delete All ps

        filtered k
            | k `elem` as = (<> fromMaybe mempty (Map.lookup All ps))
            | otherwise   = id

        ps = Map.union inPkgs $ Map.fromList (zip as (repeat mempty))

    release base (arch, ps) = liftIO $ do
        let dir  = base </> BS.unpack ("binary-" <> toByteString arch)
            rel  = dir  </> "Release"
            pkg  = dir  </> "Packages"
            pkgz = dir  </> "Packages" <.> compressExt

        createDirectoryIfMissing True dir

        -- <arch>/Release
        writeHandle rel $ \hd ->
            putBuilders hd. Pkg.toIndex $ Release inCode inOrigin inLabel arch

        -- <arch>/Packages
        writeHandle pkg $ \hd ->
            mapM_ (putBuilders hd . contents) (Set.toList ps)

        -- <arch>/Packages.<ext>
        readHandle pkg $ \x ->
            writeHandle pkgz $ \y -> runResourceT $
                Conduit.sourceHandle x
                    $= compressConduit
                    $$ Conduit.sinkHandle y

        mapM (index base) [rel, pkg, pkgz]

    index base path = Index (drop (length base + 1) path) <$> getFileStat path

    contents p@Entry{..} =
        let f = "Filename: /" +++ filename entArch entName entVers +++ "\n"
         in Pkg.toIndex p ++ [f]

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
    url = BS.unpack
        . toByteString
        $ filename entArch entName entVers

filename :: Arch -> Name -> Vers -> Builder
filename a n v = "packages/" +++ a +++ "/" +++ n +++ "/" +++ (urlEncode v)

putBuilders :: Handle -> [Builder] -> IO ()
putBuilders hd = hPutBuilder hd . joinBuilders

joinBuilders :: [Builder] -> Builder
joinBuilders = (<> "\n") . mconcat . intersperse "\n"

writeHandle :: FilePath -> (Handle -> IO a) -> IO a
writeHandle p f = withFile p WriteMode $ \hd -> do
    hSetBinaryMode hd True
    hSetBuffering hd (BlockBuffering Nothing)
    f hd

readHandle :: FilePath -> (Handle -> IO a) -> IO a
readHandle p = withFile p ReadMode
