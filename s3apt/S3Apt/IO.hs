{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : S3Apt.IO
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.IO where

import           Control.Monad
import           Data.Text                (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Directory
import           System.Exit
import           System.IO                 hiding (FilePath)

setBuffering :: IO ()
setBuffering = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

ensureExists :: FilePath -> IO ()
ensureExists (encodeString -> dir) = do
    createDirectoryIfMissing True dir
    -- Ensure any permission errors are caught.
    p <- doesDirectoryExist dir
    unless p $
        hPutStrLn stderr (dir ++ " doesnt exist.") >> exitFailure

parseOptions :: Parser a -> IO a
parseOptions p = customExecParser
    (prefs showHelpOnError)
    (info (helper <*> p) fullDesc)

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

pathOption :: Mod OptionFields String -> Parser FilePath
pathOption = fmap decodeString . strOption

debExt :: Text
debExt = ".deb"
