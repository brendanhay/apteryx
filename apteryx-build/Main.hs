{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
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

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Data.Conduit
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Network.APT.S3           as S3
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Options.Applicative
import           System.APT.IO
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package       as Pkg
import           System.APT.Types
import           System.Environment

data Options = Options
    { optFrom     :: !Key
    , optTo       :: !Key
    , optTemp     :: !Path
    , optAddress  :: Maybe Text
    , optN        :: !Int
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> keyOption
         ( long "from"
        <> metavar "BUCKET/PREFIX"
        <> help "Source S3 bucket and optional prefix to traverse for packages. [required]"
         )

    <*> keyOption
         ( long "to"
        <> metavar "BUCKET/PREFIX"
        <> help "Destination S3 bucket and optional prefix to store packages. [required]"
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
        <> help "Server to notify with new package descriptions. [default: none]"
         )

    <*> option
         ( long "concurrency"
        <> short 'c'
        <> metavar "INT"
        <> help "Maximum number of packages to process concurrently. [default: 10]"
        <> value 10
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Maximum number of most recent package versions to retain. [default: 3]"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

-- FIXME: Add verification to the build process
-- Ensure number of versions is correct, metadata can be loaded

main :: IO ()
main = do
    o@Options{..} <- parseOptions options
    name          <- Text.pack <$> getProgName
    queue         <- atomically newTQueue

    let num   = [1..optN]
        write = liftIO . atomically . writeTQueue queue

    runMain name . runAWS AuthDiscover optDebug $ do
        S3.entries name optFrom optVersions >>= mapM_ (write . Just)
        ws <- mapM (\n -> async $ worker n o queue) num
        mapM_ write (map (const Nothing) num)
        mapM_ wait ws

worker :: Int -> Options -> TQueue (Maybe Entry) -> AWS ()
worker n o q = say_ name "Starting..." >> go
  where
    name = "worker " <> Text.pack (show n)

    go = do
        mx <- liftIO . atomically $ readTQueue q
        maybe (say_ name "No more entries, exiting...")
              (\x -> build o x >> go)
              mx

build :: Options -> Entry -> AWS ()
build Options{..} Entry{..} = do
    say name "Retrieving {}" [from]
    rs   <- send $ GetObject (keyBucket from) (keyPrefix from) []
    (bdy, f) <- unwrapResumable (responseBody rs)

    say name "Parsing control from {}" [from]
    ctl  <- liftEitherT (Pkg.fromFile optTemp (aws bdy)) `finally` f

    code <- status <$> S3.copy name from ctl optTo
    say name "Completed {}" [code]
  where
    from = Key (keyBucket optFrom) entKey

    status = Text.pack . show . statusCode . responseStatus
    name   = Text.drop 1 $ Text.dropWhile (/= '/') entKey

    aws = hoist $ either throwM return <=< (`runEnv` undefined)
