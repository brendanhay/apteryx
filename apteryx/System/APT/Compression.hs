-- Module      : System.APT.Compression
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Compression where

import           Control.Monad.Trans.Resource
import qualified Codec.Compression.BZip       as BZip
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Conduit
import qualified Data.Conduit.BZlib           as Conduit

compressExt :: String
compressExt = "bz2"

compressBS :: ByteString -> ByteString
compressBS = LBS.toStrict . BZip.compress . LBS.fromStrict

decompressBS :: ByteString -> ByteString
decompressBS = LBS.toStrict . BZip.decompress . LBS.fromStrict

compressConduit :: MonadResource m => Conduit ByteString m ByteString
compressConduit = Conduit.bzip2
