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
import qualified Data.Conduit.Binary       as Conduit
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Filesystem.Path.CurrentOS as Path
import           Network.AWS
import           Options.Applicative
import           S3Apt.IO
import           S3Apt.Options
import           S3Apt.Package
import           S3Apt.S3
import           S3Apt.Types
import           System.Environment
import           System.IO

data Options = Options
    { optKey     :: !Key
    , optFile    :: !Path
    , optTemp    :: !Path
    , optAddress :: Maybe Text
    , optDebug   :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> keyOption
         ( long "key"
        <> short 'k'
        <> metavar "BUCKET/PREFIX"
        <> help "Destination S3 bucket and optional prefix to store packages. [required]"
         )

    <*> pathOption
         ( long "file"
        <> short 'f'
        <> metavar "PATH"
        <> help "Path to the file to upload. [required]"
         )

    <*> pathOption
         ( long "tmp"
        <> short 't'
        <> metavar "PATH"
        <> help "Temporary directory for unpacking Debian control files. [default: /tmp]"
        <> value "/tmp"
         )

    <*> optional (textOption
         $ long "addr"
        <> short 'a'
        <> metavar "ADDR"
        <> help "Server to notify with new package description. [default: none]"
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

main :: IO ()
main = do
    Options{..} <- parseOptions options
    name        <- Text.pack <$> getProgName
    runMain name . withFile (Path.encodeString optFile) ReadMode $ \hd -> do
        runAWS AuthDiscover optDebug $ do
            c@Control{..} <- liftEitherT
               $ loadControl optTemp (Conduit.sourceHandle hd)
              <* liftIO (hClose hd)
            upload name optKey c optFile
