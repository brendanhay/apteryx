{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.APT.S3
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.APT.S3
    ( entries
    , copy
    , upload
    , metadata
    ) where

import           Control.Error
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
import           Network.HTTP.Conduit
import           System.APT.Log
import qualified System.APT.Package        as Pkg
import           System.APT.Types

-- FIXME: pulls all of a bucket's keys into memory.
-- | Returns a set of grouped identical entries, ordered by version.
entries :: Text -> Key -> Int -> AWS [[Entry]]
entries name key n = do
    say name "Paginating contents of {}" [key]
    paginate (GetBucket (keyBucket key) (Delimiter '/') prefix 200 Nothing)
        $= Conduit.concatMap (filter match . gbrContents)
        $$ catalogue mempty
  where
    prefix =
        let pre = keyPrefix key
         in if Text.null pre then Nothing else Just pre

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

    catalogue m = await >>= maybe (return $ Map.elems m) (catalogue . entry m)

    entry m Contents{..} = Map.insertWith add (arch, pre) item m
      where
        add new old = take n . nub . sort $ new <> old

        item = [Entry (Key (keyBucket key) bcKey) end (digits ver) (fromIntegral bcSize)]

        (arch, ver)
            | Just x <- "amd64" `Text.stripSuffix` suf = (Amd64, x)
            | Just x <- "i386"  `Text.stripSuffix` suf = (I386,  x)
            | otherwise                                = (Other, suf)

        (pre, suf) = Text.break isDigit
            . fromMaybe name
            $ debExt `Text.stripSuffix` end

        end = last $ Text.split (== '/') bcKey

        digits = filter (not . Text.null)
            . map (Text.filter isDigit)
            . Text.split (`elem` "_.\\+~")

copy :: Text -> Key -> Control -> Key -> AWS PutObjectCopyResponse
copy name from c@Control{..} to = do
    say name "Copying to {}" [dest]
    send PutObjectCopy
        { pocBucket    = keyBucket dest
        , pocKey       = keyPrefix dest
        , pocSource    = src
        , pocDirective = Replace
        , pocHeaders   = Pkg.toHeaders c
        }
  where
    dest = destination to c
    src  = LText.toStrict . LText.toLazyText $ build from

upload :: Text -> Key -> Control -> Path -> AWS ()
upload name key c@Control{..} (Path.encodeString -> path) = do
    say name "Uploading {} to {}" [build path, build dest]
    bdy <- requestBodyFile path >>=
        liftEitherT . failWith (Err $ "Unable to read " ++ show path)
    send_ PutObject
        { poBucket  = keyBucket dest
        , poKey     = keyPrefix dest
        , poHeaders = Pkg.toHeaders c
        , poBody    = bdy
        }
  where
    dest = destination key c

metadata :: Text -> Key -> AWS Control
metadata name key = do
    say name "Querying {}" [key]
    hoistError . fmapL toError . Pkg.fromHeaders . responseHeaders
        =<< send HeadObject
            { hoBucket  = keyBucket key
            , hoKey     = keyPrefix key
            , hoHeaders = []
            }

destination :: Key -> Control -> Key
destination (Key b k) Control{..} = Key b $ Text.concat
    [ k
    , "/"
    , arch
    , "/"
    , pkg
    , "/"
    , pkg
    , "_"
    , Text.decodeUtf8 ctlVersion
    , "_"
    , arch
    , ".deb"
    ]
  where
    arch = Text.decodeUtf8 (toBytes ctlArch)
    pkg  = Text.decodeUtf8 ctlPackage