{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
import           Control.DeepSeq
import           Control.Error
import           Control.Exception
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8 hiding (D)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder          as Build
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.CaseInsensitive             (CI)
import           Data.Int
import           Data.List                        (intersperse)
import           Data.Map.Strict                  (Map)
import           Data.Monoid                      hiding (All)
import           Data.Set                         (Set)
import           Data.String
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.Buildable
import qualified Data.Text.Encoding               as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as LBuild
import           Data.Time
import           Data.Typeable
import           Network.AWS
import           Network.HTTP.Types.Status
import           Prelude                          hiding (all)
import           System.Locale
import           System.Logger.Message            (ToBytes(..))

debExt :: Text
debExt = ".deb"

toByteString :: ToBytes a => a -> ByteString
toByteString = LBS.toStrict . Build.toLazyByteString . bytes

fromByteString :: FromByteString a => ByteString -> Either Error a
fromByteString = fmapL (invalidField . BS.pack) . runParser parser

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

data Error where
    Exception :: SomeException -> Error
    Error     :: Status -> Builder -> Error

deriving instance Typeable Error

-- FIXME:
instance Eq Error where
    (==) a b = show a == show b

instance Show Error where
    show (Exception ex) = show ex
    show (Error    s b) = show s ++ " " ++ show (toByteString b)

instance Exception Error

instance ToError Error where
    toError (Exception ex) = toError ex
    toError e              = toError (show e)

fromError :: AWSError -> [Error]
fromError (Err  s) = [awsError s]
fromError (Ex  ex) = [Exception ex]
fromError (Ers xs) = concatMap fromError xs

instance NFData Error

invalidPackage, missingPackage, missingField, invalidField, awsError, shellError
    :: ToBytes a
    => a
    -> Error
invalidPackage = Error status409 . mappend "Invalid package: " . bytes
invalidField   = Error status409 . mappend "Invalid field: " . bytes
missingPackage = Error status409 . mappend "Missing package: " . bytes
missingField   = Error status409 . mappend "Missing field: " . bytes
awsError       = Error status500 . mappend "AWS error: " . bytes
shellError     = Error status500 . mappend "Shell error: " . bytes

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
    deriving (Eq, Ord, Show, ToBytes, IsString)

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
            (fail "Unable to parse numeric version") return
            (parseOnly (decimal `sepBy` satisfy (inClass ".+-~")) bs)

data Arch
    = All
    | Amd64
    | I386
    | ArmEL
    | ArmHF
    | HurdI386
    | KFree64
    | KFreeI386
    | Mips
    | MipsEL
    | PowerPC
    | S390X
    | Sparc
      deriving (Eq, Ord)

instance NFData Arch

instance Show Arch where
    show = BS.unpack . toByteString

instance ToBytes Arch where
    bytes All       = "all"
    bytes Amd64     = "amd64"
    bytes I386      = "i386"
    bytes ArmEL     = "armel"
    bytes ArmHF     = "armhf"
    bytes HurdI386  = "hurd-i386"
    bytes KFree64   = "kfreebsd-amd64"
    bytes KFreeI386 = "kfreebsd-i386"
    bytes Mips      = "mips"
    bytes MipsEL    = "mipsel"
    bytes PowerPC   = "powerpc"
    bytes S390X     = "s390x"
    bytes Sparc     = "sparc"

instance ToBytes [Arch] where
    bytes = mconcat . intersperse " " . map bytes

instance Buildable Arch where
    build = build . Text.decodeUtf8 . toByteString

instance FromByteString Arch where
    parser = optional (string "binary-") >> choice
        [ string "all"            >> return All
        , string "amd64"          >> return Amd64
        , string "i386"           >> return I386
        , string "armel"          >> return ArmEL
        , string "armhf"          >> return ArmHF
        , string "hurd-i386"      >> return HurdI386
        , string "kfreebsd-amd64" >> return KFree64
        , string "kfreebsd-i386"  >> return KFreeI386
        , string "mips"           >> return Mips
        , string "mipsel"         >> return MipsEL
        , string "powerpc"        >> return PowerPC
        , string "s390x"          >> return S390X
        , string "sparc"          >> return Sparc
        ]

newtype Size = Size { unSize :: Int64 }
    deriving (Eq, Ord, Show, Enum, Real, Num, Integral)

instance ToBytes Size where
    bytes = Build.int64Dec . unSize

instance FromByteString Size where
    parser = Size <$> decimal

data Stat = Stat
    { statSize   :: !Size
    , statMD5    :: !(Digest MD5)
    , statSHA1   :: !(Digest SHA1)
    , statSHA256 :: !(Digest SHA256)
    } deriving (Eq, Ord, Show)

instance NFData Stat

newtype Desc = Desc { unDesc :: ByteString }
    deriving (Eq, Ord, Show)

instance ToBytes Desc where
    bytes = bytes . unDesc

data Meta = Meta
    { metaOther :: Map (CI ByteString) ByteString
    , metaDesc  :: !Desc
    , metaStat  :: !Stat
    } deriving (Eq, Ord, Show)

instance NFData Meta

data Entry a = Entry
    { entName :: !Name
    , entVers :: !Vers
    , entArch :: !Arch
    , entAnn  :: !a
    } deriving (Eq, Ord, Show)

instance NFData a => NFData (Entry a)

instance Show a => Buildable (Entry a) where
    build = build . show

type Object  = Entry Key
type Package = Entry Meta

stat :: Package -> Stat
stat = metaStat . entAnn

sizeOf :: Package -> Int64
sizeOf = unSize . statSize . stat

mkEntry :: Name -> Vers -> Arch -> Entry ()
mkEntry n v a = Entry n v a ()

instance FromByteString Object where
    parser = takeByteString >>= either (fail "Unable to parse Entry") return . f
      where
        f k = parseOnly (p k) (last $ BS.split '/' k)

        p k = Entry
            <$> parser <* char '_'
            <*> parser <* char '_'
            <*> parser <* string (Text.encodeUtf8 debExt)
            <*> pure (Key $ Text.decodeUtf8 k)

newtype Translate a = Translate a

data Index = Index
    { idxRel  :: !FilePath
    , idxStat :: !Stat
    } deriving (Eq, Ord, Show)

instance NFData Index

data Release = Release
    { relOrigin  :: !Text
    , relLabel   :: !Text
    , relArchive :: !Text
    , relArch    :: !Arch
    } deriving (Eq, Show)

data InRelease = InRelease
    { inOrigin :: !Text
    , inLabel  :: !Text
    , inCode   :: !Text
    , inDesc   :: !Text
    , inDate   :: !UTCTime
    , inUntil  :: !UTCTime
    , inPkgs   :: Map Arch (Set Package)
    } deriving (Eq, Show)

mkInRelease :: Text
            -> Text
            -> Int
            -> (UTCTime -> Map Arch (Set Package) -> InRelease)
mkInRelease code desc h = \t ps -> InRelease
    { inOrigin = "Apteryx"
    , inLabel  = "Apteryx"
    , inCode   = code
    , inDesc   = desc
    , inDate   = t
    , inUntil  = h `addTime` t
    , inPkgs   = ps
    }

data Path
    = F !FilePath !FilePath
    | D !FilePath !FilePath
      deriving (Eq, Show)

absolute :: Path -> FilePath
absolute (F a _) = a
absolute (D a _) = a

relative :: Path -> FilePath
relative (F _ r) = r
relative (D _ r) = r

time :: UTCTime -> ByteString
time = BS.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S UTC"

addTime :: Int -> UTCTime -> UTCTime
addTime h = addUTCTime (realToFrac $ h * 60 * 60)
