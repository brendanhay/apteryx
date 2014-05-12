{-# LANGUAGE OverloadedStrings #-}

-- Module      : System.APT.Options
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.Options where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.From  (FromByteString(..))
import qualified Data.ByteString.From  as From
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           GHC.Conc
import           Options.Applicative
import           System.APT.Types

parseOptions :: Parser a -> IO a
parseOptions p = customExecParser
    (prefs $ showHelpOnError <> columns 100)
    (info (helper <*> p) $ fullDesc
       <> footer ("Concurrency:\n  " ++ show numCapabilities))

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

fromOption :: FromByteString a => Mod OptionFields a -> Parser a
fromOption = nullOption . (eitherReader (From.runParser From.parser . BS.pack) <>)

bucketOption :: Mod OptionFields String -> Parser Bucket
bucketOption = fmap (mk . Text.pack) . strOption
  where
    mk t =
        case Text.split (== '/') t of
            []     -> error "Bucket cannot be blank."
            (x:[]) -> Bucket x Nothing
            (x:xs) -> Bucket x (Just $ Text.intercalate "/" xs)
