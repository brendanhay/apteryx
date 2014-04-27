{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- Module      : System.APT.Types
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Types
    ( module System.APT.Types
    , ToBytes (..)
    ) where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Builder          as Build
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.CaseInsensitive             (CI)
import           Data.Int
import           Data.Map.Strict                  (Map)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.Buildable
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LBuild
import           Data.Typeable
import qualified Filesystem.Path.CurrentOS        as Path
import           Network.AWS
import           Network.HTTP.Types.Status
import           System.Logger.Message            (ToBytes(..))

debExt :: Text
debExt = ".deb"

toByteString :: ToBytes a => a -> ByteString
toByteString = LBS.toStrict . Build.toLazyByteString . bytes

fromByteString :: FromByteString a => ByteString -> Either Error a
fromByteString = fmapL (invalidField . BS.pack) . runParser parser

type Path = Path.FilePath

data Error where
    Exception :: SomeException -> Error
    Error     :: ToBytes a => Status -> a -> Error

deriving instance Typeable Error

instance Show Error where
    show (Exception ex) = show ex
    show (Error    s m) = show s ++ " " ++ show (toByteString m)

instance Exception Error

instance ToError Error where
    toError (Exception ex) = toError ex
    toError e              = toError (show e)

missingPackage, missingField, invalidField, awsError, shellError
    :: ToBytes a
    => a
    -> Error
missingPackage = Error status409
missingField   = Error status409
invalidField   = Error status413
awsError       = Error status500
shellError     = Error status500

data Bucket = Bucket !Text (Maybe Text)
    deriving (Eq, Show)

bktName :: Bucket -> Text
bktName (Bucket b _) = b

bktPrefix :: Bucket -> Maybe Text
bktPrefix (Bucket _ m) = m

instance Buildable Bucket where
    build (Bucket b m) = build b <> build (maybe mempty ("/" <>) m)

newtype Key = Key Text
    deriving (Eq, Show)

instance ToURL Key where
    urlEncode (Key t) = urlEncode t

instance ToBytes Key where
    bytes = Build.byteString . Text.encodeUtf8 . urlEncode

instance Buildable Key where
    build = build . urlEncode

newtype Name = Name { unName :: ByteString }
    deriving (Eq, Ord, Show, ToBytes)

instance FromByteString Name where
    parser = Name <$> takeWhile1 (/= '_')

instance Buildable Name where
    build = build . Text.decodeUtf8 . unName

data Vers = Vers
    { verRaw :: !ByteString
    , verNum :: [Int]
    } deriving (Eq, Show)

instance Ord Vers where
    a `compare` b = verNum a `compare` verNum b

instance ToURL Vers where
    urlEncode = Text.decodeUtf8 . verRaw

instance ToBytes Vers where
    bytes = Build.byteString . verRaw

instance Buildable Vers where
    build = build . Text.decodeUtf8 . verRaw

instance FromByteString Vers where
    parser = do
        bs <- takeWhile1 (/= '_')
        Vers bs <$> either
            (fail "Unable to parse numeric version components") return
            (parseOnly (decimal `sepBy` satisfy (inClass ".+-~")) bs)

data Arch
    = Other
    | Amd64
    | I386
      deriving (Eq, Ord, Show)

instance ToBytes Arch where
    bytes Other = "all"
    bytes Amd64 = "amd64"
    bytes I386  = "i386"

instance Buildable Arch where
    build Other = "all"
    build Amd64 = "amd64"
    build I386  = "i386"

instance FromByteString Arch where
    parser = (string "all"   >> return Other)
         <|> (string "amd64" >> return Amd64)
         <|> (string "i386"  >> return I386)

newtype Size = Size { unSize :: Int64 }
    deriving (Eq, Ord, Show, Num)

instance ToBytes Size where
    bytes = Build.int64Dec . unSize

type Object  = Entry Key
type Package = Entry Meta

data Entry a = Entry
    { entName :: !Name
    , entVers :: !Vers
    , entArch :: !Arch
    , entAnn  :: !a
    } deriving (Eq, Show)

instance ToURL Object where
    urlEncode = urlEncode . entAnn

instance FromByteString Object where
    parser = do
        k <- takeByteString
        either (fail "Unable to parse Entry") return
               (parseOnly (p k) (last $ BS.split '/' k))
      where
        p k = Entry
            <$> parser <* char '_'
            <*> parser <* char '_'
            <*> parser <* string (Text.encodeUtf8 debExt)
            <*> pure (Key $ Text.decodeUtf8 k)

data Meta = Meta
    { metaSize   :: !Size
    , metaMD5    :: !(Digest MD5)
    , metaSHA1   :: !(Digest SHA1)
    , metaSHA256 :: !(Digest SHA256)
    , metaOther  :: Map (CI ByteString) ByteString
    } deriving (Eq, Show)

annotate :: Entry a -> Meta -> Package
annotate o m = o { entAnn = m }

class ToURL a where
    urlEncode :: a -> Text

instance ToURL Text where
    urlEncode = LText.toStrict
        . LBuild.toLazyText
        . mconcat
        . map f
        . Text.unpack
      where
        f c | c == '+'  = "%2B"
            | c == ' '  = "%20"
            | otherwise = LBuild.singleton c
