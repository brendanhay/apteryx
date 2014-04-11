{-# LANGUAGE OverloadedStrings #-}

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

import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Byteable
import qualified Data.Text                 as Text
import           Data.Text                (Text)
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Directory
import           System.Exit
import           System.IO                 hiding (FilePath)

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
