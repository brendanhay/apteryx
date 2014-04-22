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
import           Control.Concurrent.ThreadPool
import           Data.Monoid
import           Network.AWS
import           Options.Applicative
import           System.APT.IO
import qualified System.APT.Index              as Index
import           System.APT.Options
import qualified System.APT.Package            as Pkg
import qualified System.APT.Store              as Store
import           System.APT.Types

data Options = Options
    { optFrom     :: !Bucket
    , optTo       :: !Bucket
    , optTemp     :: !Path
    , optAddress  :: Maybe String
    , optN        :: !Int
    , optVersions :: !Int
    , optDebug    :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> bucketOption
         ( long "from"
        <> metavar "BUCKET/PREFIX"
        <> help "Source S3 bucket and optional prefix to traverse for packages. [required]"
         )

    <*> bucketOption
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

    <*> optional (strOption
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
-- Multi-threaded logging

main :: IO ()
main = do
    Options{..} <- parseOptions options

    putStrLn "Looking for entries..."

    s  <- Store.new optFrom optVersions <$> loadEnv optDebug
    xs <- Store.entries s

    putStrLn $ "Found " ++ show (length xs) ++ " entries."
    putStrLn "Copying..."

    parForM optN (concat xs)
        (build s optTemp optTo)
        (const $ return ())

    maybe (return ()) Index.rebuild optAddress

    putStrLn "Done."
  where
    build s tmp dest Entry{..} = do
        ctl <- Store.get s entKey $ liftEitherT . Pkg.fromFile tmp
        Store.copy s entKey ctl dest
