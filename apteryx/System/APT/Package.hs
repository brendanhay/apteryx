{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : System.APT.Package
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Package
    (
    -- * Deserialisation
      fromFile
    , fromControl
    , fromHeaders

    -- * Serialisation
    , toIndex
    , toHeaders
    ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Base64     as Base64
import           Data.ByteString.Builder    (Builder)
import qualified Data.ByteString.Char8      as BS
import           Data.Byteable
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Conduit
import qualified Data.Conduit.Binary        as Conduit
import qualified Data.Foldable              as Fold
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Monoid
import           Network.HTTP.Types.Header
import           System.APT.Compression
import           System.APT.IO
import qualified System.APT.Package.Control as Ctl
import           System.APT.Types
import           System.IO                  (hClose)

fromFile :: MonadIO m
         => FilePath
         -> Source IO ByteString
         -> EitherT Error m Package
fromFile tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    bs <- runShell $ "ar -p " ++ path ++ " control.tar.gz | tar -Oxz ./control"
    hoistEither (Ctl.parse bs >>= fromControl) `ap` getFileHash path

fromHeaders :: FromHeaders a => [Header] -> Either Error a
fromHeaders = parseHeaders . Map.fromList . map (first stripPrefix)

class ToIndex a where
    toIndex :: a -> [Builder]

instance (ToBytes k, ToBytes v) => ToIndex (Map (CI k) v) where
    toIndex = map line . Map.toList
      where
        line (k, v) = bytes (CI.original k) <> ": " <> bytes v

instance ToIndex Stat where
    toIndex Stat{..} =
        [ "MD5sum: " =@ fromDigest statMD5
        , "SHA1: "   =@ fromDigest statSHA1
        , "SHA256: " =@ fromDigest statSHA256
        , "Size: "   =@ statSize
        ]

instance ToIndex Meta where
    toIndex Meta{..} =
        [ "Description-md5: " =@ hashDesc metaDesc
        , "Description: "     =@ metaDesc
        ] ++ toIndex metaStat

instance ToIndex Package where
    toIndex Entry{..} =
        [ "Package: "      =@ entName
        , "Version: "      =@ entVers
        , "Architecture: " =@ entArch
        ] ++ toIndex entAnn

instance ToIndex (Translate Package) where
    toIndex (Translate Entry{..}) =
        [ "Package: "         =@ entName
        , "Description-md5: " =@ hashDesc (metaDesc entAnn)
        , "Description: "     =@ metaDesc entAnn
        ]

instance ToIndex [Index] where
    toIndex xs =
           csum "MD5Sum:" statMD5
        ++ csum "SHA1:"   statSHA1
        ++ csum "SHA256:" statSHA256
      where
        csum :: Builder -> (Stat -> Digest a) -> [Builder]
        csum k f = k : map (line f) xs

        line :: (Stat -> Digest a) -> Index -> Builder
        line f x = mconcat
            [ " " =@ fromDigest (f $ idxStat x)
            , pad =@ size
            , " " =@ idxRel x
            ]
          where
            pad  = bytes (replicate (1 + (maxl - len size)) ' ')
            size = statSize (idxStat x)

        maxl = len . Fold.maximum $ map (statSize . idxStat) xs
        len  = truncate . (/ base) . log . fromIntegral

        base :: Double
        base = log 10

instance ToIndex Release where
    toIndex Release{..} =
        [ "Origin: "       =@ relOrigin
        , "Label: "        =@ relLabel
        , "Archive: "      =@ relArchive
        , "Architecture: " =@ relArch
        ]

instance ToIndex InRelease where
    toIndex InRelease{..} =
        [ "Origin: "        =@ inOrigin
        , "Label: "         =@ inLabel
        , "Codename: "      =@ inCode
        , "Date: "          =@ time inDate
        , "Valid-Until: "   =@ time inUntil
        , "Architectures: " =@ Map.keys inPkgs
        , "Description: "   =@ inDesc
        ]

class FromControl a where
    fromControl :: Map (CI ByteString) ByteString -> Either Error a

instance FromControl (Stat -> Meta) where
    fromControl m = return $
        Meta (restrict m) (Desc $ Map.findWithDefault "" "description" m)

instance FromControl (Stat -> Package) where
    fromControl m = do
        x <- fromControl m :: Either Error (Entry ())
        f <- fromControl m
        return $ (\y -> x { entAnn = y }) . f

instance FromControl (Entry ()) where
    fromControl m = Entry
        <$> (Name <$> require "package" m)
        <*> (require "version" m >>= fromByteString)
        <*> (require "architecture" m >>= fromByteString)
        <*> pure ()

class ToHeaders a where
    toHeaders :: a -> [Header]

instance ToHeaders (Map (CI ByteString) ByteString) where
    toHeaders = map (first $ mappend prefix) . Map.toList . restrict

instance ToHeaders (Digest MD5) where
    toHeaders = single "content-md5" . fromDigest

instance ToHeaders (Digest SHA1) where
    toHeaders = single (prefix <> hSHA1) . fromDigest

instance ToHeaders (Digest SHA256) where
    toHeaders = single (prefix <> hSHA256) . fromDigest

instance ToHeaders Stat where
    toHeaders Stat{..} =
           ("content-type", "application/x-deb")
         : toHeaders statMD5
        ++ toHeaders statSHA1
        ++ toHeaders statSHA256

instance ToHeaders Desc where
    toHeaders = single (prefix <> hDesc) . Base64.encode . compressBS . unDesc

instance ToHeaders Meta where
    toHeaders Meta{..} =
           toHeaders metaDesc
        ++ toHeaders metaOther
        ++ toHeaders metaStat

instance ToHeaders Name where
    toHeaders = single (prefix <> hName) . unName

instance ToHeaders Vers where
    toHeaders = single (prefix <> hVers) . verRaw

instance ToHeaders Arch where
    toHeaders = single (prefix <> hArch) . toByteString

instance ToHeaders Package where
    toHeaders Entry{..} =
           toHeaders entName
        ++ toHeaders entVers
        ++ toHeaders entArch
        ++ toHeaders entAnn

class FromHeaders a where
    parseHeaders :: Map (CI ByteString) ByteString -> Either Error a

instance FromHeaders (Map (CI ByteString) ByteString) where
    parseHeaders = Right . restrict

instance FromHeaders Size where
    parseHeaders = fromByteString <=< require "content-length"

instance FromHeaders (Digest MD5) where
    parseHeaders = toDigest . BS.init . BS.tail <=< require "etag"

instance FromHeaders (Digest SHA1) where
    parseHeaders = toDigest <=< require hSHA1

instance FromHeaders (Digest SHA256) where
    parseHeaders = toDigest <=< require hSHA256

instance FromHeaders Stat where
    parseHeaders hs = Stat
        <$> parseHeaders hs
        <*> parseHeaders hs
        <*> parseHeaders hs
        <*> parseHeaders hs

instance FromHeaders Desc where
    parseHeaders = fmap (Desc . decompressBS) . decode <=< require hDesc
      where
        decode = fmapL (invalidField . BS.pack) . Base64.decode

instance FromHeaders Meta where
    parseHeaders hs = Meta
        <$> parseHeaders hs
        <*> parseHeaders hs
        <*> parseHeaders hs

instance FromHeaders Name where
    parseHeaders = fromByteString <=< require hName

instance FromHeaders Vers where
    parseHeaders = fromByteString <=< require hVers

instance FromHeaders Arch where
    parseHeaders = fromByteString <=< require hArch

instance FromHeaders Package where
    parseHeaders hs = Entry
        <$> parseHeaders hs
        <*> parseHeaders hs
        <*> parseHeaders hs
        <*> parseHeaders hs

(=@) :: ToBytes a => Builder -> a -> Builder
(=@) k = mappend k . bytes

hName, hVers, hArch, hDesc, hSHA1, hSHA256 :: CI ByteString
hName   = "n"
hVers   = "v"
hArch   = "a"
hDesc   = "d"
hSHA1   = "1"
hSHA256 = "256"

require :: CI ByteString -> Map (CI ByteString) a -> Either Error a
require k = note (missingField $ CI.original k) . Map.lookup k

single :: k -> v -> [(k, v)]
single k = (:[]) . (k,)

toDigest :: HashAlgorithm a => ByteString -> Either Error (Digest a)
toDigest bs
    | (x, "") <- Base16.decode bs = note msg (digestFromByteString x)
    | otherwise                   = Left msg
  where
    msg = invalidField $ "Unable to decode digest: " <> bs

fromDigest :: Digest a -> ByteString
fromDigest = Base16.encode . toBytes

hashDesc :: Desc -> Builder
hashDesc d = bytes $ fromDigest (hash (unDesc d) :: Digest MD5)

restrict :: Map (CI ByteString) a -> Map (CI ByteString) a
restrict = Map.filterWithKey (\k _ -> k `elem` keys)
  where
    keys =
        [ "source"
        , "section"
        , "priority"
        , "essential"
        , "depends"
        , "installed-size"
        , "maintainer"
        , "homepage"
        , "built-using"
        , "package-type"
        ]

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix bs
    | p `BS.isPrefixOf` CI.foldedCase bs = CI.map f bs
    | otherwise = bs
  where
    f = BS.drop (BS.length p)
    p = CI.foldedCase prefix

prefix :: CI ByteString
prefix = "x-amz-meta-"
