{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : System.APT.Types
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Types where

import           Control.Applicative
import           Control.Arrow
import           Control.Exception
import           Crypto.Hash
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Builder          (byteString)
import qualified Data.ByteString.Char8            as BS
import           Data.ByteString.From
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Byteable
import           Data.CaseInsensitive             (CI)
import           Data.Int
import           Data.Map.Strict                  (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.Buildable
import qualified Data.Text.Encoding               as Text
import           Data.Typeable
import qualified Filesystem.Path.CurrentOS        as Path
import           Network.AWS
import           Network.HTTP.Types
import           System.Logger.Message            (ToBytes(..))

debExt :: Text
debExt = ".deb"

type Path = Path.FilePath

data Error
    = ShellError   Int String LBS.ByteString
    | MissingField Text
    | InvalidField Text
    | Exception    SomeException
      deriving (Show, Typeable)

instance Exception Error

instance ToError Error where
    toError (Exception ex) = toError ex
    toError e              = toError (show e)

data Arch
    = Amd64
    | I386
    | Other
      deriving (Eq, Ord, Show)

instance Byteable Arch where
    toBytes Amd64 = "amd64"
    toBytes I386  = "i386"
    toBytes Other = "all"

instance ToBytes Arch where
    bytes = byteString . toBytes

instance FromByteString Arch where
    parser = (string "amd64" >> return Amd64) <|> (string "i386" >> return I386)

archFromBS :: ByteString -> Arch
archFromBS = fromMaybe Other . fromByteString

newtype Size = Size Int64
    deriving (Eq, Ord, Show, Num)

instance Byteable Size where
    toBytes (Size n) = BS.pack (show n)

data Control = Control
    { ctlPackage  :: !ByteString
    , ctlVersion  :: !ByteString
    , ctlArch     :: !Arch
    , ctlSize     :: !Size
    , ctlMD5Sum   :: !(Digest MD5)
    , ctlSHA1     :: !(Digest SHA1)
    , ctlSHA256   :: !(Digest SHA256)
    , ctlOptional :: Map (CI ByteString) ByteString
    } deriving (Eq, Show)

data Key = Key !Text !Text
    deriving (Eq, Show)

keyBucket :: Key -> Text
keyBucket (Key b _) = b

keyPrefix :: Key -> Text
keyPrefix (Key _ k) = Text.decodeUtf8
    . BS.intercalate "/"
    . map (urlEncode True . Text.encodeUtf8)
    $ Text.split (== '/') k

instance Buildable Key where
    build k = build (keyBucket k <> "/" <> keyPrefix k)

mkKey :: Text -> Key
mkKey = uncurry Key . second f . Text.break p . f
  where
    f = Text.dropWhile p
    p = (== '/')

data Entry = Entry
    { entKey     :: !Key
    , entName    :: !Text
    , entVersion :: [Text]
    , entSize    :: !Int64
    } deriving (Eq, Show)

instance Ord Entry where
    a `compare` b = f a `compare` f b
      where
        f Entry{..} = Down (entName, entVersion)
