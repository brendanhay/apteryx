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
import           Control.Concurrent.Async   (waitCatch)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import           Data.Conduit
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Options.Applicative
import           System.APT.IO
import           System.APT.Log
import           System.APT.Options
import           System.APT.Package
import           Network.APT.S3
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
        <> help "Maximum number of most recent package versions to store. [default: 3]"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

main :: IO ()
main = do
    o@Options{..} <- parseOptions options
    name          <- Text.pack <$> getProgName
    q             <- atomically newTQueue
    runMain name . runAWS AuthDiscover optDebug $ do
        contents name optFrom optVersions >>=
            mapM_ (liftIO . atomically . writeTQueue q)
        ws <- mapM (\n -> async $ worker n o q) [1..optN]
        rs <- mapM (liftIO . waitCatch) ws
        maybe (return ()) hoistError (listToMaybe $ rights rs)

worker :: Int -> Options -> TQueue Entry -> AWS ()
worker n o q = say_ name "Starting..." >> go
  where
    go = do
        mx <- liftIO . atomically $ tryReadTQueue q
        maybe (say_ name "Exiting {}...")
              (\x -> build o x >> go)
              mx

    name = "worker " <> Text.pack (show n)

build :: Options -> Entry -> AWS ()
build Options{..} Entry{..} = do
    say name "Retrieving {}" [from]
    rs   <- send $ GetObject (keyBucket from) (keyPrefix from) []
    (bdy, f) <- unwrapResumable (responseBody rs)

    say name "Parsing control from {}" [from]
    ctl  <- liftEitherT (loadControl optTemp (aws bdy)) `finally` f

    code <- status <$> copy name from ctl optTo
    say name "Completed {}" [code]
  where
    from = Key (keyBucket optFrom) entKey

    status = Text.pack . show . statusCode . responseStatus
    name   = Text.drop 1 $ Text.dropWhile (/= '/') entKey

    aws = hoist $ either throwM return <=< (`runEnv` undefined)
