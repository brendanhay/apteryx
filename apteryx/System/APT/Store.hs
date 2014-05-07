{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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
    , raw
    , copy
    , entries
    , metadata
    , presign

    , objectKey
    ) where

import           Control.Applicative
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
import           Data.Time
import           Network.AWS.S3            hiding (Bucket, Source)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method
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
add :: MonadIO m => Package -> FilePath -> Store -> m ()
add p path s = aws s $
    send_ PutObject
        { poBucket  = bucketName s
        , poKey     = objectKey p s
        , poHeaders = Pkg.toHeaders p
        , poBody    = requestBodySource size (Conduit.sourceFile path)
        }
  where
    size = unSize . statSize $ stat p

get :: (MonadIO m, Keyed a)
    => Entry a
    -> (Source IO ByteString -> AWS b)
    -> Store
    -> m (Maybe b)
get o f s = aws s $ do
    e  <- getEnv
    rs <- sendCatch GetObject
        { goBucket  = bucketName s
        , goKey     = objectKey o s
        , goHeaders = []
        }
    maybe (return Nothing)
          (\x -> do
              (bdy, g) <- unwrapResumable (responseBody x)
              (Just <$> f (hoist_ e bdy)) `finally` g)
          (hush rs)
  where
    hoist_ e = hoist $ either throwM return <=< runEnv e

raw :: (MonadIO m, Keyed a)
    => Entry a
    -> Store
    -> m (Source IO ByteString, IO ())
raw o s = aws s $ do
    e  <- getEnv
    rs <- send GetObject
        { goBucket  = bucketName s
        , goKey     = objectKey o s
        , goHeaders = []
        }
    (bdy, g) <- unwrapResumable (responseBody rs)
    return ( hoist (either throwM return <=< runEnv e) bdy
           , runEnv e g >>= either throwM return
           )

-- | Lookup the metadata for a specific entry.
metadata :: (MonadIO m, Keyed a) => Entry a -> Store -> m (Maybe Package)
metadata o s = aws s $ do
    rs <- hush <$> sendCatch HeadObject
        { hoBucket  = bucketName s
        , hoKey     = objectKey o s
        , hoHeaders = []
        }
    maybe (return Nothing)
          (return . hush . Pkg.fromHeaders . responseHeaders)
          rs

-- | Copy an object from the store, to another location in S3,
--   overriding the metadata with the supplied package description.
copy :: MonadIO m => Object -> Package -> Bucket -> Store -> m ()
copy from to bkt s = aws s $
    send_ PutObjectCopy
        { pocBucket    = bktName bkt
        , pocKey       = objectKey to s
        , pocSource    = bucketName s <> "/" <> objectKey from s
        , pocDirective = Replace
        , pocHeaders   = Pkg.toHeaders to
        }

-- | Get a list of objects starting at the store's prefix,
--   adhering to the version limit and returning a list ordered by version.
entries :: MonadIO m => Store -> m [[Object]]
entries s = aws s $ paginate start
    $= Conduit.concatMap (filter match . gbrContents)
    $$ catalogue mempty
  where
    start  = GetBucket (bucketName s) (Delimiter '/') prefix 250 Nothing
    prefix = bktPrefix (_bucket s)

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

    catalogue m = maybe (return $ Map.elems m) (catalogue . entry m)
        =<< await

    entry m Contents{..} =
        maybe m (\x -> Map.insertWith insert (entArch x, entName x) [x] m)
                (BS.fromByteString (Text.encodeUtf8 bcKey))

    insert xs = take (_versions s)
        . sortBy (compare `on` (Down . entVers))
        . mappend xs

presign :: (MonadIO m, Keyed a) => Entry a -> UTCTime -> Store -> m ByteString
presign e t s = aws s $ presignS3 GET bkt key t
  where
    bkt = Text.encodeUtf8 $ bucketName s
    key = Text.encodeUtf8 $ objectKey e s

-- | Run an AWS action using the store's environment.
-- FIXME: proper error handling
aws :: MonadIO m => Store -> AWS a -> m a
aws s m = liftIO (runAWSEnv (_env s) m) >>= either (error . show) return

bucketName :: Store -> Text
bucketName = bktName . _bucket

bucketPrefix :: Text -> Store -> Text
bucketPrefix t s = strip prefix <> "/" <> strip t
  where
    prefix  = fromMaybe "" . bktPrefix $ _bucket s
    strip x = fromMaybe x $ "/" `Text.stripSuffix` x

class Keyed a where
    objectKey :: Entry a -> Store -> Text
    objectKey Entry{..} = bucketPrefix ("pool/" <> urlEncode path)
      where
        path = Key $ Text.concat
            [ name
            , "/"
            , name
            , "_"
            , Text.decodeUtf8 (verRaw entVers)
            , "_"
            , Text.decodeUtf8 (toByteString entArch)
            , ".deb"
            ]

        name = Text.decodeUtf8 (unName entName)

instance Keyed (a, b)
instance Keyed ()

instance Keyed Key where
    objectKey = const . urlEncode . entAnn
