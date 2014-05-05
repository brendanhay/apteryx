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

import           Control.Applicative              hiding (optional)
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base16           as Base16
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From             (FromByteString)
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

toHeaders :: Package -> [Header]
toHeaders p@Entry{..} =
    [ ("content-md5",  base64 statMD5)
    , ("content-type", "application/x-deb")
    ] ++ [ "package"      =: entName
         , "version"      =: entVers
         , "architecture" =: toByteString entArch
         , "sha1"         =: base16 statSHA1
         , "sha256"       =: base16 statSHA256
         ]
  where
    (=:) k = (CI.mk headerPrefix <> k,) . toByteString

    Stat{..} = stat p

fromHeaders :: [Header] -> Either Error Package
fromHeaders xs = fromMap hs `ap` st
  where
    st = Stat
        <$> (require "content-length" hs >>= field Size decimal)
        <*> digest "etag" (BS.init . BS.tail)
        <*> digest "sha1" id
        <*> digest "sha256" id

    digest k f = require k hs >>= \e ->
        let bs  = fst . Base16.decode $ f e
            msg = "Unable to read digest: " <> CI.original k <> " " <> bs
         in note (invalidField msg)
                 (digestFromByteString bs)

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
         => FilePath
         -> Source IO ByteString
         -> EitherT Error m Package
fromFile tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    bs <- runShell $ "ar -p " ++ path ++ " control.tar.gz | tar -Ox control"
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

headerPrefix :: ByteString
headerPrefix = "x-amz-meta-"

stripPrefix :: CI ByteString -> CI ByteString
stripPrefix = CI.map f
  where
    f x | headerPrefix `BS.isPrefixOf` x = BS.drop (BS.length headerPrefix) x
        | otherwise                      = x
