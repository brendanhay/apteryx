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
    ( toBuilder
    , toHeaders
    , fromHeaders
    , fromMap
    , fromFile
    ) where

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
import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder          as Build
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From             (FromByteString)
import           Data.Byteable
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Char                        (isAlpha, toUpper)
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.List                        (intersperse)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid
import qualified Filesystem.Path.CurrentOS        as Path
import           Network.HTTP.Types.Header
import           System.APT.IO
import           System.APT.Types
import           System.IO                        (hClose)

-- Binary packages:
-- They always follow a rigid naming convention: package-name_version_arch.deb.

toBuilder :: Package -> Builder
toBuilder Package{..} = (<> "\n") . mconcat . intersperse "\n" $
    [ "Package: "      =@ pkgName
    , "Version: "      =@ pkgVersion
    , "Architecture: " =@ toByteString pkgArch
    , "Size: "         =@ toByteString pkgSize
    , "MD5Sum: "       =@ base16 pkgMD5Sum
    , "SHA1: "         =@ base16 pkgSHA1
    , "SHA256: "       =@ base16 pkgSHA256
    ] ++ map (Build.byteString . line) (Map.toList pkgOptional)
  where
    (=@) k = mappend k . bytes

    line (k, v) = upcase (CI.original k) <> ": " <> v

    upcase k
        | Just (c, bs) <- BS.uncons k = toUpper c `BS.cons` bs
        | otherwise                   = k

toHeaders :: Package -> [Header]
toHeaders Package{..} =
    [ ("content-md5",  base64 pkgMD5Sum)
    , ("content-type", "application/x-deb")
    ] ++ [ "package"      =@ pkgName
         , "version"      =@ pkgVersion
         , "architecture" =@ toByteString pkgArch
         , "sha1"         =@ base16 pkgSHA1
         , "sha256"       =@ base16 pkgSHA256
         ]
  where
    (=@) k = (CI.mk headerPrefix <> k,) . toByteString

fromHeaders :: [Header] -> Either Error Package
fromHeaders xs = join $ fromMap hs
    <$> size "content-length"
    <*> digest "etag" (BS.init . BS.tail)
    <*> digest "sha1" id
    <*> digest "sha256" id
  where
    size k = require k hs >>= field Size decimal

    digest k f = require k hs >>= \e ->
        let bs  = fst . Base16.decode $ f e
            msg = "Unable to read digest: " <> CI.original k <> " " <> bs
         in note (invalidField msg)
                 (digestFromByteString bs)

    hs = Map.fromList $ map (first stripPrefix) xs

fromMap :: Map (CI ByteString) ByteString
        -> Size
        -> Digest MD5
        -> Digest SHA1
        -> Digest SHA256
        -> Either Error Package
fromMap fs size md5 sha1 sha256 = Package
    <$> require "package" fs
    <*> require "version" fs
    <*> require "architecture" fs
    <*> pure size
    <*> pure md5
    <*> pure sha1
    <*> pure sha256
    <*> pure fields
  where
    fields = Map.map (BS.take 1024)
        $ Map.filterWithKey (const . (`elem` optional)) fs

    optional =
        [ "source"
        , "section"
        , "priority"
        , "essential"
        , "depends"
        , "installed-size"
        , "maintainer"
        , "description"
        , "homepage"
        , "built-using"
        , "package-type"
        ]

fromFile :: MonadIO m
         => Path
         -> Source IO ByteString
         -> EitherT Error m Package
fromFile tmp src = withTempFileT tmp ".deb" $ \path hd -> do
    catchErrorT $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    let f = Path.encodeString path
    bs <- runShellT $ "ar -p " ++ f ++ " control.tar.gz | tar -Ox control"
    fs <- hoistEither (fields bs)
    hoistEither =<< fromMap fs
        <$> getFileSizeT path
        <*> hashFileT path
        <*> hashFileT path
        <*> hashFileT path
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

base16 :: Digest a -> ByteString
base16 = Base16.encode . toBytes

base64 :: Digest a -> ByteString
base64 = Base64.encode . toBytes

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

headerPrefix :: ByteString
headerPrefix = "x-amz-meta-"

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix = CI.map f
  where
    f x | headerPrefix `BS.isPrefixOf` x = BS.drop (BS.length headerPrefix) x
        | otherwise                      = x
