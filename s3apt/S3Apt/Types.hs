{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- Module      : S3Apt.Types
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.Types where

import           Control.Arrow
import           Control.Exception
import           Crypto.Hash
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Byteable
import           Data.Int
import           Data.Map.Strict            (Map)
import           Data.Monoid
import           Data.Ord
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Buildable
import qualified Filesystem.Path.CurrentOS  as Path
import           Network.AWS

debExt :: Text
debExt = ".deb"

type Path = Path.FilePath

data Error
    = ShellError   Int String LBS.ByteString
    | MissingField Text
    | InvalidField Text
    | Exception    SomeException
      deriving (Show)

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

archFromBS :: ByteString -> Arch
archFromBS "amd64" = Amd64
archFromBS "i386"  = I386
archFromBS _       = Other

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
    , ctlOptional :: Map ByteString ByteString
    } deriving (Eq, Show)

data Key = Key { keyBucket :: !Text, keyPrefix :: !Text }
    deriving (Eq, Show)

instance Buildable Key where
    build Key{..} = build (keyBucket <> "/" <> keyPrefix)

mkKey :: Text -> Key
mkKey = uncurry Key . second f . Text.break p . f
  where
    f = Text.dropWhile p
    p = (== '/')

data Entry = Entry
    { entKey     :: !Text
    , entName    :: !Text
    , entVersion :: [Text]
    , entSize    :: !Int64
    } deriving (Eq, Show)

instance Ord Entry where
    a `compare` b = f a `compare` f b
      where
        f Entry{..} = Down (entName, entVersion)
