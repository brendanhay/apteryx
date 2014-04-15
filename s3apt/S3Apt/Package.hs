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
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Filesystem.Path.CurrentOS        hiding (stripPrefix)
import           Network.HTTP.Types
import           Prelude                          hiding (FilePath)
import           S3Apt.IO
import           S3Apt.Text
import           S3Apt.Types
import           System.IO                        (hClose)

loadControl :: MonadIO m
            => Text
            -> FilePath
            -> Source IO ByteString
            -> EitherT Error m Control
loadControl pre tmp src = withTempFile tmp ".deb" $ \path hd -> do
    catchError $ (src $$ Conduit.sinkHandle hd) >> hClose hd
    bs <- runShell $
        "ar -p " ++ encodeString path ++ " control.tar.gz | tar -Ox control"
    fs <- hoistEither (parseFields bs)
    hoistEither =<< parseControl pre fs
        <$> getFileSize path
        <*> hashFile path
        <*> hashFile path
        <*> hashFile path

parseControl :: Text
             -> Map ByteString ByteString
             -> Size
             -> Digest MD5
             -> Digest SHA1
             -> Digest SHA256
             -> Either Error Control
parseControl pre fs size md5 sha1 sha256 = do
    p <- require "Package"
    v <- require "Version"
    a <- require "Architecture"
    return $
        Control (file p v a) p v (archFromBS a) size md5 sha1 sha256 fields
  where
    file pkg ver arch = Text.decodeUtf8 $ BS.concat
        [ Text.encodeUtf8 $ stripPrefix "/" pre
        , "/"
        , urlEncode True pkg
        , "/"
        , urlEncode True pkg
        , "_"
        , urlEncode True ver
        , "_"
        , arch
        , ".deb"
        ]

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
