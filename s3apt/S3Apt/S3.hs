{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : S3Apt.S3
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.S3
    ( contents
    , copy
    , upload
    ) where

import           Control.Error
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as Base64
import           Data.Byteable
import           Data.Char                 (isDigit)
import           Data.Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.List                 (sort, nub)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Buildable
import qualified Data.Text.Encoding        as Text
import qualified Data.Text.Lazy            as LText
import qualified Data.Text.Lazy.Builder    as LText
import qualified Filesystem.Path.CurrentOS as Path
import           Network.AWS.S3            hiding (Bucket)
import           Network.HTTP.Types
import           S3Apt.Log
import           S3Apt.Types

-- FIXME: pulls all of a bucket's keys into memory.
contents :: Text -> Key -> Int -> AWS [Entry]
contents name k@Key{..} n = do
    say name "Paginating contents of {}" [k]
    paginate (GetBucket keyBucket (Delimiter '/') prefix 200 Nothing)
        $= Conduit.concatMap (filter match . gbrContents)
        $$ catalogue mempty
  where
    prefix = if Text.null keyPrefix then Nothing else Just keyPrefix

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

    catalogue m = await >>=
        maybe (return . concat $ Map.elems m)
              (catalogue . entry m)

    entry m Contents{..} = Map.insertWith add (arch, key) item m
      where
        add new old = take n . nub . sort $ new <> old

        (arch, ver)
            | Just x <- "amd64" `Text.stripSuffix` suf = (Amd64, x)
            | Just x <- "i386"  `Text.stripSuffix` suf = (I386,  x)
            | otherwise                                = (Other, suf)

        (key, suf) = Text.break isDigit
            . fromMaybe name
            $ debExt `Text.stripSuffix` end

        item = [Entry bcKey end (digits ver) (fromIntegral bcSize)]
        end  = last $ Text.split (== '/') bcKey

        digits = filter (not . Text.null)
            . map (Text.filter isDigit)
            . Text.split delim

        delim = flip elem "_.\\+~"

copy :: Text -> Key -> Control -> Key -> AWS PutObjectCopyResponse
copy name from c@Control{..} to = do
    say name "Copying {} to {}" [from, to]
    send $ PutObjectCopy
        { pocBucket    = keyBucket
        , pocKey       = keyPrefix
        , pocSource    = source
        , pocDirective = Replace
        , pocHeaders   = metadata c
        }
  where
    Key{..} = destination to c
    source  = LText.toStrict . LText.toLazyText $ build from

upload :: Text -> Key -> Control -> Path -> AWS ()
upload name key c@Control{..} (Path.encodeString -> path) = do
    say name "Uploading {}" [path]
    bdy <- requestBodyFile path >>=
        liftEitherT . failWith (Err $ "Unable to read " ++ show path)
    send_ PutObject
        { poBucket  = keyBucket
        , poKey     = keyPrefix
        , poHeaders = metadata c
        , poBody    = bdy
        }
  where
    Key{..} = destination key c

metadata :: Control -> [Header]
metadata Control{..} =
    [ ("Content-MD5",  md5)
    , ("Content-Type", "application/x-deb")
    ] ++ [ "Package"      =@ ctlPackage
         , "Version"      =@ ctlVersion
         , "Architecture" =@ toBytes ctlArch
         , "Size"         =@ toBytes ctlSize
         , "MD5"          =@ md5
         , "SHA1"         =@ base64 ctlSHA1
         , "SHA256"       =@ base64 ctlSHA256
         ]
  where
    (=@) k = ("x-amz-meta-" <> k,)

    md5 = base64 ctlMD5Sum

    base64 :: Byteable a => a -> ByteString
    base64 = Base64.encode . toBytes

destination :: Key -> Control -> Key
destination Key{..} Control{..} = Key keyBucket $ Text.concat
    [ keyPrefix
    , "/"
    , arch
    , "/"
    , pkg
    , "/"
    , pkg
    , "_"
    , encode ctlVersion
    , "_"
    , arch
    , ".deb"
    ]
  where
    arch = encode (toBytes ctlArch)
    pkg  = encode ctlPackage

encode :: ByteString -> Text
encode = Text.decodeUtf8 . urlEncode True
