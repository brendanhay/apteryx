{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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
    -- ( Index
    -- , new

    -- , path
    -- , lookup
    -- , trySync
    -- , syncIfMissing
    -- , sync

    -- , rebuild
    -- , reindex
    -- ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Crypto.Hash.Conduit       as Crypto
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8     as BS
import qualified Data.Foldable             as Fold
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                 (Text)
import qualified Data.Text as Text
import           Data.Time
import           Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem.Path.CurrentOS as Path
import           Network.HTTP.Conduit      hiding (path)
import           Prelude                   hiding (lookup)
import           System.APT.IO
import qualified System.APT.Package        as Pkg
import           System.APT.Store          (Store)
import qualified System.APT.Store          as Store
import           System.APT.Types
import           System.Directory
import           System.IO
import           System.IO.Temp
import           System.Process

import           Control.Exception (throwIO)
import           System.Exit

import Control.Monad.Par.IO
import Control.Monad.Par.Combinator

latest :: Text -> Text -> Text -> Text -> Store -> IO InRelease
latest org lbl code desc s = do
    es <- Store.entries s
    t  <- getCurrentTime
    ps <- runParIO $ parMapM (Fold.foldrM getMeta mempty) es
    return InRelease
        { relOrigin = org
        , relLabel  = lbl
        , relCode   = code
        , relDate   = t
        , relUntil  = t
        , relDesc   = desc
        , relPkgs   = Map.unionsWith (<>) ps
        }
  where
     -- getMeta :: Object
     --         -> Map Text (Map Arch (Set Package))
     --         -> IO (Map Text (Map Arch (Set Package)))
     getMeta x m = do
         p <- Store.metadata x s
         return $ maybe m insertComp p

     insertComp :: Package
                -> Map Text (Map Arch (Set Package))
     insertComp p = Fold.foldl' (flip ins) mempty (comps p)
       where
         ins :: Text
             -> Map Text (Map Arch (Set Package))
             -> Map Text (Map Arch (Set Package))
         ins k = Map.insertWith (Map.unionWith (<>)) k arch

         arch :: Map Arch (Set Package)
         arch = Map.singleton (entArch p) (Set.singleton p)

write :: Path -> Path -> InRelease -> IO ()
write (Path.encodeString -> tmp) dest r@InRelease{..} = do
    ensureExists dest

    withTempDirectory tmp "apt." $ \path -> do
        ids <- runParIO $ parMapM (parComp path) (Map.toList relPkgs)

        write (path ++ "/Release") $ \hd -> do
            hPutBuilder hd (bytes r)
            hPutBuilder hd (bytes $ concat $ concat ids)

        _ <- rawSystem "cp" ["-rf", path ++ "/", Path.encodeString dest ++ "/"]

        return ()
  where
    write p f = withFile p WriteMode $ \hd -> do
        hSetBinaryMode hd True
        hSetBuffering hd (BlockBuffering Nothing)
        f hd

    parComp base (name, as) = do
        let dir = base ++ "/" ++ Text.unpack name
            ctl = Control relOrigin relLabel name

        liftIO $ createDirectory dir

        parMapM (parArch dir ctl) (Map.toList as)

    parArch base ctl (arch, ps) = liftIO $ do
        let dir = base ++ "/binary-" ++ BS.unpack (toByteString arch)

            rel  = dir ++ "/Release"
            pkg  = dir ++ "/Packages"
            pkgz = dir ++ "/Packages.xz"

        createDirectory dir

        write rel $ \hd -> hPutBuilder hd (bytes $ ctl arch)
        write pkg $ \hd -> mapM_ (hPutBuilder hd . bytes) (Set.toList ps)

        forM [rel, pkg] $
            \(Path.decodeString -> x) -> Index x <$> getFileStat x

    -- components base m = forM (Map.toList m) $ \(comp, as) -> do

    --     forM (Map.toList as) $ \(arch, ps) -> do
    --         print (arch, Set.size ps)

        
        -- let base   = Path.encodeString . Path.append path

        -- parMapM n es $ \xs -> do
        --     ps <- mapM (`Store.metadata` s) xs
        --     hPutBuilder hd (Fold.concatMap bytes ps)
        --     foldl' (\m x -> Map.insertWith (<>) "main" (entArch x) m) mempty ps

        -- is <- forM (Map.toList relComps) $ \(cmp, arch) ->
        --     forM (sub cmp arch) $ \(c, ps, dir) -> do
        --         let rel  = dir </> "Release"
        --             pkg  = dir </> "Packages"
        --             pkgz = dir </> "Packages.xz"

        --         createDirectoryIfMissing True (base dir)

        --         wf rel $ \hd -> hPutBuilder hd (bytes c)
        --         wf pkg $ \hd -> mapM_ (hPutBuilder hd . bytes) ps

        --         -- Packages.xz
        --         forM [rel, pkg] $ \x ->
        --             Index x <$> getFileSizeT (base x)
        --                     <*> Crypto.hashFile (base x)

  --       wf "Release" $ \hd -> do
  --           hPutBuilder hd (bytes r)
  --           mapM_ (hPutBuilder hd . bytes) (concat is)

  --       c  <- rawSystem "mv"
  --           [ Path.encodeString path ++ "/"
  --           , Path.encodeString dest ++ "/"
  --           ]

  --       case c of
  --           ExitSuccess   -> return ()
  --           ExitFailure n -> error "Shit!"

  --   -- Copy to InRelease
  --   -- Sign InRelease
  --   -- Atomically replace directory
  -- where
  --   sub c = map (ctl c) . Map.toList

  --   ctl c (a, ps) =
  --       let d = Path.fromText relCode
  --            </> Path.fromText c
  --            </> Path.decodeString (show a)
  --        in (Control relOrigin relLabel c a, ps, d)

-- -- FIXME: Move to a more relevant location

-- -- | Trigger a remote rebuild of the index.
-- rebuild :: MonadIO m => String -> m ()
-- rebuild host = liftIO $ do
--     rq <- parseUrl addr
--     _  <- withManager $ httpLbs (rq { method = "POST" })
--     return ()
--   where
--     addr = host <> "/packages"

-- -- | Trigger a remote reindex of a specified package.
-- reindex :: MonadIO m => Package -> String -> m ()
-- reindex Entry{..} host = liftIO $ do
--     rq <- parseUrl addr
--     void . withManager $ httpLbs (rq { method = "PATCH" })
--   where
--     addr = host <> url

--     url = BS.unpack $ mconcat
--         [ "/packages/"
--         , toByteString entArch
--         , "/"
--         , toByteString entName
--         , "/"
--         , toByteString (urlEncode entVers)
--         ]
