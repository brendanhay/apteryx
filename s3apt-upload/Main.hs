{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Main
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Monad.IO.Class
import qualified Data.Conduit.Binary        as Conduit
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Filesystem.Path.CurrentOS  as Path
import           Network.AWS
import           Options.Applicative
import           S3Apt.IO
import           S3Apt.Log
import           S3Apt.Package
import           S3Apt.S3
import           S3Apt.Types
import           System.Environment
import           System.Exit
import           System.IO

data Options = Options
    { optBucket :: !Text
    , optPrefix :: Maybe Text
    , optPath   :: !Path.FilePath
    , optTemp   :: !Path.FilePath
    , optDebug  :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> textOption
         ( long "bucket"
        <> short 'b'
        <> metavar "BUCKET"
        <> help "S3 bucket to store packages."
         )

    <*> optional (textOption
         $ long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "Additional S3 key prefix to prepend. default: ''"
         )

    <*> pathOption
         ( long "file"
        <> short 'f'
        <> metavar "PATH"
        <> help "File to upload."
         )

    <*> pathOption
         ( long "tmp"
        <> short 't'
        <> metavar "PATH"
        <> help "Temporary directory. default: /tmp"
        <> value "/tmp"
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

main :: IO ()
main = do
    Options{..} <- parseOptions options
    name <- Text.pack <$> getProgName

    setBuffering
    say_ name "Starting..."

    let path = Path.encodeString optPath
        pre  = fromMaybe "" optPrefix

    e <- withFile path ReadMode $ \hd -> do
        runAWS AuthDiscover optDebug $ do
            c@Control{..} <- liftEitherT
               $ loadControl pre optTemp (Conduit.sourceHandle hd)
              <* liftIO (hClose hd)

            upload optBucket c optPath

    either (\ex -> hPrint stderr ex >> exitFailure)
           (const $ say_ name "Completed." >> exitSuccess)
           e
