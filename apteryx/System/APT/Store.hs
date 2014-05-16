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
    , run
    , parMapM

    , add
    , get
    , copy
    , entries
    , metadata
    , presign

    , ToKey (..)
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Error
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader
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
import qualified System.APT.IO             as IO
import qualified System.APT.Package        as Pkg
import           System.APT.Types

-- FIXME: Retries should be a f or n to relevant functions

data Env = Env
    { _bucket   :: !Bucket
    , _versions :: !Int
    , _aws      :: !AWSEnv
    }

bucketName :: Store Text
bucketName = bktName <$> asks _bucket

bucketPrefix :: Store (Maybe Text)
bucketPrefix = bktPrefix <$> asks _bucket

type Store = ReaderT Env AWS

run :: Bucket -> Int -> AWSEnv -> Store a -> IO (Either AWSError a)
run b v e = run' (Env b v e)

run' :: Env -> Store a -> IO (Either AWSError a)
run' e s = runAWSEnv (_aws e) (runReaderT s e)

-- Some sort of error handling here? MonadCatch?
parMapM :: NFData b => (a -> Store b) -> [a] -> Store ([AWSError], [b])
parMapM f xs = do
    e <- ask
    partitionEithers <$> IO.parMapM (liftIO . run' e . f) xs

-- | Get a list of objects starting at the store's prefix,
--   adhering to the version limit and returning a list ordered by version.
entries :: Store [[Object]]
entries = do
    v  <- asks _versions
    rq <- GetBucket
        <$> bucketName
        <*> pure (Delimiter '/')
        <*> bucketPrefix
        <*> pure 250
        <*> pure Nothing
    lift $ paginate rq
        $= Conduit.concatMap (filter match . gbrContents)
        $$ catalogue v mempty
  where
    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

    catalogue v m = maybe (return $ Map.elems m) (catalogue v . entry v m)
        =<< await

    entry v m Contents{..} =
        maybe m (\x -> Map.insertWith (insert v) (entArch x, entName x) [x] m)
                (BS.fromByteString (Text.encodeUtf8 bcKey))

    insert v xs = take v . sortBy (compare `on` (Down . entVers)) . mappend xs

-- | Lookup the metadata for a specific entry.
metadata :: ToKey a => a -> Store Package
metadata k = do
    key <- toKey k
    rs  <- lift . send =<< HeadObject
        <$> bucketName
        <*> pure key
        <*> pure []
    either throwM return (Pkg.fromHeaders $ responseHeaders rs)

-- | Given a package description and contents, upload the file to S3.
add :: Package -> FilePath -> Store ()
add p f = lift . send_ =<< PutObject
    <$> bucketName
    <*> toKey p
    <*> pure (Pkg.toHeaders p)
    <*> pure (requestBodySource (sizeOf p) (Conduit.sourceFile f))

get :: ToKey a => a -> (Source IO ByteString -> AWS b) -> Store (Maybe b)
get o f = do
    e  <- asks _aws
    rq <- GetObject <$> bucketName <*> toKey o <*> pure []
    rs <- lift $ sendCatch rq
    maybe (return Nothing)
          (\x -> lift $ do
              (bdy, g) <- unwrapResumable (responseBody x)
              (Just <$> f (hoist_ e bdy)) `finally` g)
          (hush rs)
  where
    hoist_ e = hoist $ either throwM return <=< runAWSEnv e

-- | Copy an object from the store, to another location in S3,
--   overriding the metadata with the supplied package description.
copy :: Object -> Package -> Bucket -> Store ()
copy from to bkt = do
    b  <- bucketName
    kt <- toKey to
    kf <- toKey from
    lift . send_ $
        PutObjectCopy (bktName bkt) kt (b <> "/" <> kf) Replace (Pkg.toHeaders to)

presign :: ToKey a => a -> UTCTime -> Store ByteString
presign k t = lift =<< presignS3 GET
    <$> (Text.encodeUtf8 <$> bucketName)
    <*> (Text.encodeUtf8 <$> toKey k)
    <*> pure t

class ToKey a where
    toKey :: a -> Store Text

instance ToKey Key where
    toKey = return . urlEncode

instance ToKey (Entry Key) where
    toKey x = toKey (entAnn x)

instance ToKey (Entry Meta) where
    toKey = objectKey

instance ToKey (Entry ()) where
    toKey = objectKey

objectKey :: Entry a -> Store Text
objectKey Entry{..} = f (urlEncode path)
  where
    path = Text.concat
        [ "pool/"
        , name
        , "/"
        , name
        , "_"
        , Text.decodeUtf8 (verRaw entVers)
        , "_"
        , Text.decodeUtf8 (toByteString entArch)
        , ".deb"
        ]

    name = Text.decodeUtf8 (unName entName)

    f :: Text -> Store Text
    f x = (<> "/" <> g x) . g . fromMaybe "" <$> bucketPrefix

    g :: Text -> Text
    g x = fromMaybe x $ "/" `Text.stripSuffix` x
