-- Module      : System.APT.Index
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Interface to the in-memory index of package metadata.
module System.APT.Index where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import qualified Filesystem.Path.CurrentOS as Path
import           System.APT.Store (Store)
import qualified System.APT.Store as Store
import           System.APT.Types

type Key = (Arch, Name, [Text])

data Index = Index
    { _store :: Store
    , _map   :: IORef (Map Key [Package])
    }

-- | Create an index from the given store.
new :: MonadIO m => Store -> m Index
new s = Index s `liftM` liftIO (newIORef mempty)

-- | Rebuild the index.
rebuild :: MonadIO m => Index -> m Index
rebuild = undefined

-- | Lookup a specific package.
lookup :: MonadIO m => Arch -> Name -> Vers -> [Text] -> m (Maybe Package)
lookup = undefined

-- | Insert a package into the index, replacing any existing description.
insert :: MonadIO m => Index -> Package -> m ()
insert = undefined
