{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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

import           Control.Exception
import           Crypto.Hash
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Byteable
import           Data.Int
import           Data.Map.Strict            (Map)
import           Data.Text                  (Text)
import           Network.AWS

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
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Byteable Size where
    toBytes (Size n) = BS.pack (show n)

data Control = Control
    { ctlFilename :: !Text
    , ctlPackage  :: !ByteString
    , ctlVersion  :: !ByteString
    , ctlArch     :: !Arch
    , ctlSize     :: !Size
    , ctlMD5Sum   :: !(Digest MD5)
    , ctlSHA1     :: !(Digest SHA1)
    , ctlSHA256   :: !(Digest SHA256)
    , ctlOptional :: Map ByteString ByteString
    } deriving (Eq, Show)

data Error
    = ShellError   Int String LBS.ByteString
    | MissingField Text
    | InvalidField Text
    | Exception    SomeException
      deriving (Show)

instance ToError Error where
    toError (Exception ex) = toError ex
    toError e              = toError (show e)
