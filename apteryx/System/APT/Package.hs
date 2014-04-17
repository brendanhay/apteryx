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
    ( toHeaders
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
import qualified Data.ByteString.Base64           as Base64
import qualified Data.ByteString.Char8            as BS
import           Data.Byteable
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Char                        (isAlpha)
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Monoid
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Filesystem.Path.CurrentOS        as Path
import           Network.HTTP.Types.Header
import           System.APT.IO
import           System.APT.Types
import           System.IO                        (hClose)

toHeaders :: Control -> [Header]
toHeaders Control{..} =
    [ ("content-md5",  base64 ctlMD5Sum)
    , ("content-type", "application/x-deb")
    ] ++ [ "package"      =@ ctlPackage
         , "version"      =@ ctlVersion
         , "architecture" =@ toBytes ctlArch
         , "sha1"         =@ base64 ctlSHA1
         , "sha256"       =@ base64 ctlSHA256
         ]
  where
    (=@) k = (CI.mk headerPrefix <> k,)

    base64 :: Byteable a => a -> ByteString
    base64 = Base64.encode . toBytes

fromHeaders :: [Header] -> Either Error Control
fromHeaders xs = join $ fromMap hs
    <$> size "content-length"
    <*> digest "etag"
    <*> digest "sha1"
    <*> digest "sha256"
  where
    size k   = require k hs >>= field Size decimal
    digest k = require k hs >>=
        note (InvalidField . Text.decodeUtf8 $ "Unable to read digest" <> CI.original k)
            . digestFromByteString

    hs = Map.fromList $ map (first stripPrefix) xs

fromMap :: Map (CI ByteString) ByteString
        -> Size
        -> Digest MD5
        -> Digest SHA1
        -> Digest SHA256
        -> Either Error Control
fromMap fs size md5 sha1 sha256 = Control
    <$> require "package" fs
    <*> require "version" fs
    <*> (archFromBS <$> require "architecture" fs)
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
            -> EitherT Error m Control
fromFile tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    let f = Path.encodeString path
    bs <- runShell $ "ar -p " ++ f ++ " control.tar.gz | tar -Ox control"
    fs <- hoistEither (fields bs)
    hoistEither =<< fromMap fs
        <$> getFileSize path
        <*> hashFile path
        <*> hashFile path
        <*> hashFile path
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

require :: CI ByteString
        -> Map (CI ByteString) ByteString
        -> Either Error ByteString
require k m =
    maybe (Left . MissingField . Text.decodeUtf8 $ CI.original k) Right
          (Map.lookup k m)

field :: (a -> b) -> Parser a -> ByteString -> Either Error b
field f p = fmapL (InvalidField . Text.pack) . fmap f . parseOnly p

headerPrefix :: ByteString
headerPrefix = "x-amz-meta-"

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix = CI.map f
  where
    f x | headerPrefix `BS.isPrefixOf` x = BS.drop (BS.length headerPrefix) x
        | otherwise                      = x
