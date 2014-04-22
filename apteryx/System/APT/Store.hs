{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : System.APT.Store
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Interface to the storage and retrieval of singular/plural artifacts from S3.
module System.APT.Store
    ( Store
    , new
    , add
    , get
    , copy
    , entries
    , metadata
    ) where

import           Control.Error
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.From      as BS
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.Function             (on)
import           Data.List                 (sortBy)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Network.AWS.S3            hiding (Bucket, Source)
import           Network.HTTP.Conduit
import qualified System.APT.Package        as Pkg
import           System.APT.Types

-- | Structure:
--
-- +-arbitrary prefix
--   +-pool
--     +-package-name
--       +-*.deb
--   +-override
--     +-[components]
--       +-package-name
--         +-*.deb

data Store = Store
    { _bucket   :: !Bucket
    , _versions :: !Int
    , _env      :: AWSEnv
    }

-- | Create a new S3 backed store.
new :: Bucket -> Int -> AWSEnv -> Store
new = Store

-- | Given a package description and contents, upload the file to S3.
add :: MonadIO m => Store -> Package -> [Text] -> Path -> m ()
add s pkg cs path = aws s $
    send_ PutObject
        { poBucket  = bkt
        , poKey     = key
        , poHeaders = Pkg.toHeaders pkg
        , poBody    = requestBodySource size (Conduit.sourceFile file)
        }
  where
    (bkt, key) = location (_bucket s) pkg cs

    size = unSize $ pkgSize pkg
    file = Path.encodeString path

get :: (MonadThrow m, MonadIO m)
    => Store
    -> Object
    -> (Source IO ByteString -> AWS a)
    -> m a
get s o f = aws s $ do
    e  <- getEnv
    rs <- send GetObject
        { goBucket  = bktBucket
        , goKey     = bktPrefix <> "/" <> objKey o
        , goHeaders = []
        }

    (bdy, g) <- unwrapResumable (responseBody rs)

    f (hoist_ e bdy) `finally` g
  where
    Bucket{..} = _bucket s

    hoist_ e = hoist $ either throwM return <=< runEnv e

-- | Copy an object from the store, to another location in S3,
--   overriding the metadata with the supplied package description.
copy :: MonadIO m => Store -> Object -> Package -> Bucket -> m ()
copy s o pkg to = aws s $
    send_ PutObjectCopy
        { pocBucket    = bkt
        , pocKey       = key
        , pocSource    = src
        , pocDirective = Replace
        , pocHeaders   = Pkg.toHeaders pkg
        }
  where
    -- FIXME: components need to be taken into account
    (bkt, key) = location to pkg []

    src  = bktBucket (_bucket s) <> "/" <> objKey o

-- | Get a flattened list of entries starting at the store's prefix,
--   adhering to the version limit and returning a list ordered by version.
entries :: MonadIO m => Store -> m [[Entry]]
entries s = aws s $ paginate start
    $= Conduit.concatMap (filter match . gbrContents)
    $$ catalogue mempty
  where
    Bucket{..} = _bucket s

    start = GetBucket bktBucket (Delimiter '/') prefix 200 Nothing

    prefix =
        let pre = bktPrefix
        in if Text.null pre then Nothing else Just pre

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

    catalogue m = await >>= maybe (return $ Map.elems m) (catalogue . entry m)

    entry m Contents{..} =
        case BS.fromByteString (Text.encodeUtf8 bcKey) of
            Nothing -> m
            Just f  ->
                let x = f (fromIntegral bcSize :: Size)
                 in Map.insertWith insert (entArch x, entName x) [x] m

    insert xs = take (_versions s)
        . sortBy (compare `on` (Down . entVersion))
        . mappend xs

-- | Lookup the metadata for a specific entry.
metadata :: MonadIO m => Store -> Object -> m (Maybe Package)
metadata s o = aws s $ do
    rs <- send HeadObject
        { hoBucket  = bktBucket
        , hoKey     = bktPrefix <> "/" <> objKey o
        , hoHeaders = []
        }
    return . hush . Pkg.fromHeaders $ responseHeaders rs
  where
    Bucket{..} = _bucket s

-- FIXME: proper error handling
-- | Run an AWS action using the store's environment.
aws :: MonadIO m => Store -> AWS a -> m a
aws s m = liftIO (runAWSEnv (_env s) m) >>= either (error . show) return

-- | Given a package description, return the raw bucket and object key
--   of the file in S3.
location :: Bucket -> Package -> [Text] -> (Text, Text)
location Bucket{..} Package{..} cs = (bktBucket, file)
  where
    file = Text.concat
        [ root
        , name
        , "/"
        , name
        , "_"
        , urlEncode . Text.decodeUtf8 $ verRaw pkgVersion
        , "_"
        , Text.decodeUtf8 (toByteString pkgArch)
        , ".deb"
        ]

    name = Text.decodeUtf8 (unName pkgPackage)

    root = mappend bktPrefix $
        case cs of
            [] -> "/pool/"
            _  -> "/override/" <> Text.intercalate "/" cs <> "/"