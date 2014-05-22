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
    ( ToKey
    , storeKey

    , Store
    , run
    , guard
    , parMapM

    , semantic
    , versioned

    , metadata
    , get
    , add
    , copy
    , presign
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Error
import           Control.Monad             hiding (guard)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.From      as BS
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import qualified Data.Foldable             as Fold
import           Data.Function             (on)
import           Data.List                 (sortBy)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Data.Time                 as Time
import           Network.AWS.S3            hiding (Bucket, Source)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method
import qualified System.APT.IO             as IO
import qualified System.APT.Package        as Pkg
import           System.APT.Types

class ToKey a where
    storeKey :: a -> Store Text

instance ToKey Text where
    storeKey = return

instance ToKey Contents where
    storeKey = return . bcKey

instance ToKey Key where
    storeKey = return . urlEncode

instance ToKey Object where
    storeKey x = storeKey (entAnn x)

instance ToKey Package where
    storeKey = entryKey

instance ToKey Upload where
    storeKey = entryKey

data Env = Env
    { _bucket   :: !Bucket
    , _versions :: !Int
    , _aws      :: !AWSEnv
    }

type Store = ReaderT Env AWS

runStore :: Env -> Store a -> IO (Either AWSError a)
runStore e s = runAWSEnv (_aws e) (runReaderT s e)

run :: Bucket -> Int -> AWSEnv -> Store a -> IO (Either AWSError a)
run b v e = runStore (Env b v e)

guard :: Store a -> Store (Either AWSError a)
guard s = do
    env <- ask
    liftIO $ runStore env s

parMapM :: NFData b => (a -> Store b) -> [a] -> Store [b]
parMapM f xs = do
    env      <- ask
    (es, ys) <- partitionEithers <$> IO.parMapM (liftIO . runStore env . f) xs
    mapM_ throwM es
    return ys

-- | Get a list of objects starting at the store's prefix, semantically named,
--   and adhering to the version limit and returning a list ordered by version.
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
        $= Conduit.concatMap filterContents
        $$ catalogue v mempty
  where
    catalogue v m = do
        x <- await
        maybe (return . Map.elems $ Map.map Set.fromList m)
              (catalogue v . insert v m)
              x

    insert v m Contents{..} =
        let f xs = take v . sortBy (compare `on` (Down . entVers)) . mappend xs
            g x  = Map.insertWith f (entArch x, entName x) [x] m
         in maybe m g . BS.fromByteString $ Text.encodeUtf8 bcKey

-- | Lookup the metadata for a specific entry.
metadata :: ToKey a => a -> Store Package
metadata k = do
    key <- storeKey k
    rs  <- lift . send =<< HeadObject
        <$> bucketName
        <*> pure key
        <*> pure []
    either throwM return (Pkg.fromHeaders $ responseHeaders rs)

get :: ToKey a => a -> (Source IO ByteString -> AWS b) -> Store (Maybe b)
get o f = do
    e  <- asks _aws
    rq <- GetObject <$> bucketName <*> storeKey o <*> pure []
    rs <- lift $ sendCatch rq
    maybe (return Nothing)
          (\x -> lift $ do
              (bdy, g) <- unwrapResumable (responseBody x)
              (Just <$> f (hoist_ e bdy)) `finally` g)
          (hush rs)
  where
    hoist_ e = hoist $ either throwM return <=< runAWSEnv e

-- | Given a package description and contents, upload the file to S3.
add :: Package -> FilePath -> Store ()
add p f = lift . send_ =<< PutObject
    <$> bucketName
    <*> storeKey p
    <*> pure (Pkg.toHeaders p)
    <*> pure (requestBodySource (sizeOf p) (Conduit.sourceFile f))

-- | Copy an object from the store, to another location in S3,
--   overriding the metadata with the supplied package description.
copy :: ToKey a => a -> Package -> Bucket -> Store ()
copy from to bkt = do
    b  <- bucketName
    kt <- storeKey to
    kf <- storeKey from
    lift . send_ $
        PutObjectCopy (bktName bkt) kt (b <> "/" <> kf) Replace (Pkg.toHeaders to)

presign :: ToKey a => a -> Int -> Store ByteString
presign k sec = lift =<< presignS3 GET
    <$> (Text.encodeUtf8 <$> bucketName)
    <*> (Text.encodeUtf8 <$> storeKey k)
    <*> liftIO (Time.addUTCTime (realToFrac sec) <$> Time.getCurrentTime)

filterContents :: GetBucketResponse -> [Contents]
filterContents = filter match . gbrContents

match :: Contents -> Bool
match Contents{..}
    | bcSize == 0               = False
    | bcStorageClass == Glacier = False
    | otherwise                 = debExt `Text.isSuffixOf` bcKey

class ToKey a where
    toKey :: a -> Store Text

instance ToKey Text where
    toKey = return

instance ToKey Key where
    toKey = return . urlEncode

instance ToKey (Entry Key) where
    toKey x = toKey (entAnn x)

instance ToKey (Entry Meta) where
    toKey = objectKey

bucketPrefix :: Store (Maybe Text)
bucketPrefix = bktPrefix <$> asks _bucket

entryKey :: Entry a -> Store Text
entryKey Entry{..} = f (urlEncode path)
  where
    path = Text.concat
        [ name
        , "/"
        , name
        , "_"
        , Text.decodeUtf8 (toByteString entArch)
        , ".deb"
        ]

    name = Text.decodeUtf8 (unName entName)

    f :: Text -> Store Text
    f x = (<> "/" <> g x) . g . fromMaybe "" <$> bucketPrefix

    g :: Text -> Text
    g x = fromMaybe x $ "/" `Text.stripSuffix` x
