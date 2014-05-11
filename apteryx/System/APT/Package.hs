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
    , fromHeaders

    -- * Serialisation
    , toIndex
    , toHeaders
    ) where

import qualified Codec.Compression.Zlib           as ZLib
import           Control.Applicative              hiding (optional)
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base16           as Base16
import qualified Data.ByteString.Base64           as Base64
import qualified Data.ByteString.Base64.Lazy      as LBase64
import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From             (FromByteString)
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Char                        (isAlpha)
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid
import           Network.HTTP.Types.Header
import           System.APT.IO
import           System.APT.Types
import           System.IO                        (hClose)

-- FIXME: Description is not being parsed correctly

fromFile :: MonadIO m
         => FilePath
         -> Source IO ByteString
         -> EitherT Error m Package
fromFile tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    bs <- runShell $ "ar -p " ++ path ++ " control.tar.gz | tar -Oxz ./control"
    hoistEither (fields bs >>= fromControl) `ap` getFileStat path
  where
    fields = field (Map.fromList . map (first CI.mk)) parser

    parser = many' $ (,)
        <$> (takeWhile1 isAlpha <* char ':' <* many1 space)
        <*> (BS.pack <$> manyTill anyChar end)

    end = (`unless` fail "") =<< choice
        [ string "\n " >> return False
        , endOfLine >> return True
        , endOfInput >> return True
        ]

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
        [ "MD5sum: " =@ base16 statMD5
        , "SHA1: "   =@ base16 statSHA1
        , "SHA256: " =@ base16 statSHA256
        , "Size: "   =@ statSize
        ]

instance ToIndex Meta where
    toIndex Meta{..} =
        [ "Description: "     =@ metaDesc
        , "Description-md5: " =@ base16 (hash metaDesc :: Digest MD5)
        ] ++ toIndex metaStat

instance ToIndex Package where
    toIndex Entry{..} =
        [ "Package: "      =@ entName
        , "Version: "      =@ entVers
        , "Architecture: " =@ entArch
        ] ++ toIndex entAnn

class FromControl a where
    fromControl :: Map (CI ByteString) ByteString -> Either Error a

instance FromControl (Stat -> Meta) where
    fromControl m = return $ Meta desc other
      where
        desc  = fromMaybe "" $ Map.lookup "description" m
        other = Map.filterWithKey f $ restrict m

        f "description"     _ = False
        f "description-md5" _ = False
        f _                 _ = True

instance FromControl (Stat -> Package) where
    fromControl m = do
        f <- Entry
            <$> require "package" m
            <*> require "version" m
            <*> require "architecture" m
        g <- fromControl m
        return $ f . g

class ToHeaders a where
    toHeaders :: a -> [Header]

instance ToBytes v => ToHeaders (Map (CI ByteString) v) where
    toHeaders = map (uncurry (=:)) . Map.toList

instance ToHeaders Stat where
    toHeaders Stat{..} =
        [ "content-md5"  =- base64 statMD5
        , "content-type" =- ("application/x-deb" :: ByteString)
        , hSHA1          =: base64 statSHA1
        , hSHA256        =: base64 statSHA256
        ]

instance ToHeaders Meta where
    toHeaders Meta{..} =
        (hDesc =: gz) : toHeaders metaOther ++ toHeaders metaStat
      where
        gz = LBase64.encode . ZLib.compress $ LBS.fromStrict metaDesc

instance ToHeaders Package where
    toHeaders Entry{..} =
        [ hName =: entName
        , hVers =: entVers
        , hArch =: toByteString entArch
        ] ++ toHeaders entAnn

(=-) :: ToBytes a => k -> a -> (k, ByteString)
(=-) k = (k,) . toByteString

(=:) :: ToBytes a => CI ByteString -> a -> (CI ByteString, ByteString)
(=:) k = (CI.mk prefix <> k,) . toByteString

class FromHeaders a where
    parseHeaders :: Map (CI ByteString) ByteString -> Either Error a

instance FromHeaders Stat where
    parseHeaders hs = Stat
        <$> (require "content-length" hs >>= field Size decimal)
        <*> digest "etag" etag
        <*> digest hSHA1 b64
        <*> digest hSHA256 b64
      where
        b64 = fmapL invalidField . Base64.decode

        etag x | (y, "") <- Base16.decode . BS.init $ BS.tail x = Right y
               | otherwise = Left (invalidField $ "Unable to decode ETag: " <> x)

        msg = invalidField . mappend "Unable to read header: " . CI.original

        digest k f = do
            x <- require k hs
            either Left
                   (note (msg k) . digestFromByteString)
                   (f x)

instance FromHeaders Meta where
    parseHeaders hs = Meta desc other <$> parseHeaders hs
      where
        desc = LBS.toStrict
            . ZLib.decompress
            . LBS.fromStrict
            . fromMaybe ""
            $ Map.lookup hDesc hs >>= hush . Base64.decode

        other = Map.filterWithKey (\k _ -> k /= hDesc) (restrict hs)

instance FromHeaders Package where
    parseHeaders hs = Entry
        <$> require hName hs
        <*> require hVers hs
        <*> require hArch hs
        <*> parseHeaders hs

hName, hVers, hArch, hDesc, hSHA1, hSHA256 :: CI ByteString
hName   = "n"
hVers   = "v"
hArch   = "a"
hDesc   = "d"
hSHA1   = "1"
hSHA256 = "256"

require :: FromByteString a
        => CI ByteString
        -> Map (CI ByteString) ByteString
        -> Either Error a
require k m =
    maybe (Left . missingField $ CI.original k)
          fromByteString
          (Map.lookup k m)

field :: (a -> b) -> Parser a -> ByteString -> Either Error b
field f p = fmapL (invalidField . BS.pack) . fmap f . parseOnly p

restrict :: Map (CI ByteString) ByteString -> Map (CI ByteString) ByteString
restrict = Map.filterWithKey (\k _ -> k `elem` optional)
  where
    optional =
        [ "source"
        , "section"
        , "priority"
        , "essential"
        , "depends"
        , "installed-size"
        , "maintainer"
        , "description"
        , "description-md5"
        , "homepage"
        , "built-using"
        , "package-type"
        ]

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix = CI.map f
  where
    f x | prefix `BS.isPrefixOf` x = BS.drop (BS.length prefix) x
        | otherwise                      = x

prefix :: ByteString
prefix = "x-amz-meta-"
