-- Module      : S3Apt.Text
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.Text where

import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as Text

stripPrefix :: Text -> Text -> Text
stripPrefix x y = fromMaybe y (Text.stripPrefix x y)

stripSuffix :: Text -> Text -> Text
stripSuffix x y = fromMaybe y (Text.stripSuffix x y)
