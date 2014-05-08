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
    -- * Serialisation
      toHeaders

    -- * Deserialisation
    , fromHeaders
    , fromMap
    , fromFile
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
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From             (FromByteString)
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Char                        (isAlpha)
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.List                        (partition)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import           Data.Monoid
import           Network.HTTP.Types.Header
import           System.APT.IO
import           System.APT.Types
import           System.IO                        (hClose)

toHeaders :: Package -> [Header]
toHeaders p@Entry{..} =
    [ ("content-md5",  base64 statMD5)
    , ("content-type", "application/x-deb")
    ] ++ [ "package"         =: entName
         , "version"         =: entVers
         , "architecture"    =: toByteString entArch
         , "sha1"            =: base64 statSHA1
         , "sha256"          =: base64 statSHA256
         , "description-gz"  =: LBase64.encode zip
         , "description-md5" =: base16 md5
         ] ++ map (uncurry (=:)) fs
  where
    Stat{..} = stat p

    (=:) k = (CI.mk headerPrefix <> k,) . toByteString

    zip = ZLib.compress (LBS.fromStrict desc)
    md5 = hash desc :: Digest MD5

    desc = case md of
        Just (_, x) -> x
        Nothing     -> ""

    (md, fs) = first listToMaybe
        . partition ((== "description") . fst)
        . Map.toList
        . restrict
        $ meta p

fromHeaders :: [Header] -> Either Error Package
fromHeaders xs = fromMap hs `ap` st
  where
    st = Stat
        <$> (require "content-length" hs >>= field Size decimal)
        <*> digest "etag" (BS.init . BS.tail)
        <*> digest "sha1" id
        <*> digest "sha256" id

    digest k f = require k hs >>= \e ->
        let msg = "Unable to read digest: " <> CI.original k
         in either (Left . invalidField)
                   (note (invalidField msg) . digestFromByteString)
                   (Base64.decode $ f e)

    hs = Map.fromList $ map (first stripPrefix) xs

fromMap :: Map (CI ByteString) ByteString -> Either Error (Stat -> Package)
fromMap fs = do
    e <- Entry
        <$> require "package" fs
        <*> require "version" fs
        <*> require "architecture" fs
        <*> pure ()
    return $ \st -> annotate st fields e
  where
    fields = restrict $
        case Map.lookup "description-gz" fs of
            Nothing -> fs
            Just x  ->
                either (const $ Map.delete "description-md5" fs)
                       (\v -> Map.insert "description" v fs)
                       (ungzip <$> Base64.decode x)

    ungzip = LBS.toStrict . ZLib.decompress . LBS.fromStrict

fromFile :: MonadIO m
         => FilePath
         -> Source IO ByteString
         -> EitherT Error m Package
fromFile tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    bs <- runShell $ "ar -p " ++ path ++ " control.tar.gz | tar -Oxz ./control"
    hoistEither (fields bs >>= fromMap) `ap` getFileStat path
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

headerPrefix :: ByteString
headerPrefix = "x-amz-meta-"

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix = CI.map f
  where
    f x | headerPrefix `BS.isPrefixOf` x = BS.drop (BS.length headerPrefix) x
        | otherwise                      = x
