{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : S3Apt.S3
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.S3 where

import           Control.Error
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as Base64
import           Data.Byteable
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as Text
import           Filesystem.Path.CurrentOS hiding (stripPrefix)
import           Network.AWS.S3            hiding (Bucket)
import           Network.HTTP.Types
import           Prelude                   hiding (FilePath)
import           S3Apt.Text
import           S3Apt.Types

upload :: Text -> Control -> FilePath -> AWS ()
upload bkt Control{..} path = do
    bdy <- requestBodyFile (encodeString path) >>=
        liftEitherT . failWith (Err $ "Unable to read " ++ show path)
    send_ PutObject
        { poBucket  = bkt
        , poKey     = ctlFilename
        , poHeaders = headers
        , poBody    = bdy
        }
  where
    headers =
        [ ("Content-MD5",  enc ctlMD5Sum)
        , ("Content-Type", "application/x-deb")
        ] ++
        [ "Package"      =@ ctlPackage
        , "Version"      =@ ctlVersion
        , "Architecture" =@ toBytes ctlArch
        , "Size"         =@ toBytes ctlSize
        , "MD5"          =@ enc ctlMD5Sum
        , "SHA1"         =@ enc ctlSHA1
        , "SHA256"       =@ enc ctlSHA256
        ]

    (=@) k = ("x-amz-meta-" <> k,)

    enc :: Byteable a => a -> ByteString
    enc = Base64.encode . toBytes

encodeKey :: Text -> Text
encodeKey = Text.decodeUtf8
    . urlEncode True
    . Text.encodeUtf8
    . stripPrefix "/"
