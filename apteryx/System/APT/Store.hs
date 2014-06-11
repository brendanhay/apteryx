{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# LANGUAGE DefaultSignatures       #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
    ( ToKey (..)
    , Store
    , run
    , parMapM

    , semantic
    , versioned

    , metadata
    , get
    , add
    , copy
    , presign
    , monotonic
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Error
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Control.Monad.Trans.Reader
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.From       as BS
import           Data.Conduit
import qualified Data.Conduit.Binary        as Conduit
import qualified Data.Conduit.List          as Conduit
import qualified Data.Foldable              as Fold
import           Data.List                  (sort)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Time                  as Time
import           Network.AWS.S3             hiding (Bucket, Source)
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method
import qualified System.APT.Package         as Pkg
import           System.APT.Types

default (ByteString)

class ToKey a where
    objectKey :: Bucket -> a -> Text

instance ToKey Contents where
    objectKey _ = bcKey

instance ToKey Version where
    objectKey _ = vKey

instance ToKey (Entry ()) where
    objectKey = entryKey

instance ToKey Object where
    objectKey b = prependPrefix b . urlEncode . entAnn

instance ToKey Package where
    objectKey = entryKey

data Env = Env
    { _max :: !Int
    , _aws :: !AWSEnv
    }

type Store = ReaderT Env AWS

runStore :: Env -> Store a -> IO (Either AWSError a)
runStore e s = runAWSEnv (_aws e) (runReaderT s e)

run :: Int -> AWSEnv -> Store a -> IO (Either AWSError a)
run v e = runStore (Env v e)

parMapM :: NFData b => (a -> Store b) -> [a] -> Store [b]
parMapM f xs = do
    e        <- ask
    (es, ys) <- partitionEithers <$> mapM (liftIO . runStore e . f) xs
    mapM_ throwM es
    return ys

-- | Get a list of objects starting semantically named from a specific bucket,
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
        let f xs = take v . sort . mappend xs
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

get :: ToKey k
    => Bucket
    -> k
    -> (Source IO ByteString -> AWS a)
    -> Store (Maybe a)
get b k f = do
    e  <- asks _aws
    rs <- lift . sendCatch $ GetObject (bktName b) (objectKey b k) []
    maybe (return Nothing)
          (\x -> lift $ do
              (bdy, g) <- unwrapResumable (responseBody x)
              (Just <$> f (hoist_ e bdy)) `finally` g)
          (hush rs)
  where
    hoist_ e = hoist $ either throwM return <=< runAWSEnv e

add :: Bucket -> Package -> FilePath -> Store ()
add b k f = lift . send_ $ PutObject (bktName b) (objectKey b k) hs bdy
  where
    hs  = Pkg.toHeaders k
    bdy = requestBodySource (sizeOf k) (Conduit.sourceFile f)

copy :: ToKey k => Bucket -> k -> Bucket -> Package -> Store ()
copy bf kf bt kt = lift . send_ $ PutObjectCopy (bktName bt) dst src Replace hs
  where
    dst = objectKey bt kt
    src = strip (bktName bf) <> strip (objectKey bf kf)
    hs  = Pkg.toHeaders kt

presign :: ToKey k => Bucket -> k -> Int -> Store ByteString
presign b k n = liftIO expiry >>= lift . presignS3 GET bkt obj
  where
    expiry = Time.addUTCTime (realToFrac n) <$> Time.getCurrentTime

    bkt = Text.encodeUtf8 (bktName b)
    obj = Text.encodeUtf8 (objectKey b k)

monotonic :: ToKey (Entry a) => Bucket -> Entry a -> Store b -> Store (Maybe b)
monotonic b k s = do
    rs <- lift (sendCatch $ HeadObject (bktName b) (objectKey b k) []) >>= parse
    case rs of
--        Left  e | serStatus e /= Just 404 -> throwM (toError e)
        Right x | entVers x >= entVers k  -> return Nothing
        _                                 -> Just <$> s
  where
    parse :: Either e HeadObjectResponse -> Store (Either e Package)
    parse (Left  e) = return (Left e)
    parse (Right r) = either throwM (return . Right) . Pkg.fromHeaders $
        responseHeaders r

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

prependPrefix :: Bucket -> Text -> Text
prependPrefix b k = "/" <> f
  where
    f | Just x <- bktPrefix b
      , not (Text.null x) = strip x <> "/" <> strip k
      | otherwise         = strip k

strip :: Text -> Text
strip x = f . Text.stripSuffix "/" . f $ Text.stripPrefix "/" x
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
