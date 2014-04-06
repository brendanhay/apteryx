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

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List                 (isSuffixOf)
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           S3Apt.IO
import           System.Directory
import           System.FSNotify
import           System.Posix.Signals

data Options = Options
    { optBucket   :: !Text
    , optPrefix   :: Maybe Text
    , optIncoming :: !Text
    , optN        :: !Int
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> textOption
         ( long "bucket"
        <> short 'b'
        <> metavar "BUCKET"
        <> help "S3 bucket to store packages in."
         )

    <*> optional (textOption
         $ long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "Additional prefix for S3 keys."
         )

    <*> textOption
         ( long "incoming"
        <> short 'i'
        <> metavar "PATH"
        <> help "Incoming directory for packages. default: incoming"
        <> value "incoming"
         )

    <*> option
         ( long "concurrency"
        <> short 'c'
        <> metavar "INT"
        <> help "Maximum number of concurrent downloads. default: 10"
        <> value 4
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Number versions to allow in the package index. default: 3"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

data Task
    = Exit
    | Task FilePath

main :: IO ()
main = do
    Options{..} <- parseOptions options

    setBuffering
    ensureExists optIncoming

    let dir = Text.unpack optIncoming
        ext = Text.unpack debExt

    -- Work queue
    q  <- atomically $ newTBQueue optN

    -- Start workers
    ws <- mapM (const . async $ worker q) [1..optN]

    -- Watch incoming dir for added files
    m  <- startManager
    watchTree m (fromText optIncoming) added (watch q)

    -- Populate the work queue with an existing .debs
    filter (isSuffixOf ext) <$> getDirectoryContents dir >>= mapM_ (enqueue q)

    -- Exit gracefully on signal
    void $ installHandler keyboardSignal (shutdown q optN) Nothing

    -- Wait for all workers to exit
    void $ mapM_ wait ws

    stopManager m
  where
    added (Added _ _) = True
    added _           = False

    watch q (Added p _) = atomically . writeTBQueue q $ Task p
    watch _ _           = return ()

    enqueue q = atomically . writeTBQueue q . Task . decodeString

    shutdown q n = Catch
        . atomically
        $ mapM_ (const $ writeTBQueue q Exit) [1..n]

worker :: TBQueue Task -> IO ()
worker queue = do
    f <- msg . drop 9 . show <$> myThreadId
    f "started."
    go f
  where
    go f = do
        x <- atomically $ readTBQueue queue
        case x of
            Exit   -> f "exiting..."
            Task p -> f ("processing " ++ show p) >> threadDelay 1000000 >> go f

    msg t s = putStrLn $ "Worker " ++ t ++ " " ++ s
