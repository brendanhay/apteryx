{-# LANGUAGE OverloadedStrings #-}

-- Module      : S3Apt.Package
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.Package
    ( loadControl
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.CaseInsensitive
import           Data.Char                        (isAlpha)
import           Data.Conduit
import qualified Data.Conduit.Binary              as Conduit
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import qualified Filesystem.Path.CurrentOS        as Path
import           S3Apt.IO
import           S3Apt.Types
import           System.IO                        (hClose)

loadControl :: MonadIO m
            => Path
            -> Source IO ByteString
            -> EitherT Error m Control
loadControl tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    let f = Path.encodeString path
    bs <- runShell $ "ar -p " ++ f ++ " control.tar.gz | tar -Ox control"
    fs <- hoistEither (parseFields bs)
    hoistEither =<< parseControl fs
        <$> getFileSize path
        <*> hashFile path
        <*> hashFile path
        <*> hashFile path

parseControl :: Map ByteString ByteString
             -> Size
             -> Digest MD5
             -> Digest SHA1
             -> Digest SHA256
             -> Either Error Control
parseControl fs size md5 sha1 sha256 = Control
    <$> require "Package"
    <*> require "Version"
    <*> (archFromBS <$> require "Architecture")
    <*> pure size
    <*> pure md5
    <*> pure sha1
    <*> pure sha256
    <*> pure fields
  where
    require k =
        maybe (Left . MissingField $ Text.decodeUtf8 k)
              Right
              (Map.lookup k fs)

    fields = Map.map (BS.take 1024)
        $ Map.filterWithKey (\k -> const $ mk k `notElem` reserved) fs

    reserved =
        [ "Package"
        , "Version"
        , "Architecture"
        , "Filename"
        , "Size"
        , "MD5Sum"
        , "SHA1"
        , "SHA256"
        ]

parseFields :: ByteString -> Either Error (Map ByteString ByteString)
parseFields = fmapL (InvalidField . Text.pack)
    . fmap Map.fromList
    . P.parseOnly fields
  where
    fields = many' $ (,)
        <$> (P.takeWhile1 isAlpha <* char ':' <* many1 space)
        <*> (BS.pack <$> manyTill anyChar stop)

    stop = (`unless` fail "") =<< choice
        [ P.string "\n " >> return False
        , P.endOfLine >> return True
        , endOfInput >> return True
        ]
