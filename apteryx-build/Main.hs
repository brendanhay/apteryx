{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
import           Control.Concurrent.ThreadPool
import           Data.ByteString               (ByteString)
import           Data.Monoid
import           Network.AWS
import           Options.Applicative
import           System.APT.IO
import qualified System.APT.Index              as Index
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package            as Pkg
import qualified System.APT.Store              as Store
import           System.APT.Types
import           System.Environment

default (ByteString)

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

-- FIXME:
--   Add verification to the build process
--   Ensure number of versions is correct, metadata can be loaded

main :: IO ()
main = do
    Options{..} <- parseOptions options

    n  <- getProgName

    say n "Looking for entries in {}..." [optFrom]

    s  <- Store.new optFrom optVersions <$> loadEnv optDebug
    xs <- Store.entries s

    say n "Discovered {} package groups." [length xs]

    mapM_ (\e -> say n "Found {}" [objKey $ entKey e]) (concat xs)

    say n "Copying to {}..." [optTo]

    parForM optN (concat xs)
        (worker s optTemp optTo)
        (const $ return ())

    maybe (return ())
          (\e -> do
              say n "Triggering rebuild of {}" [optAddress]
              Index.rebuild e)
          optAddress

    say_ n "Done."
  where
    worker s tmp dest Entry{..} = do
        n <- (mappend "worker-") . drop 9 . show <$> myThreadId

        say n "Retreiving {}" [entKey]
        ctl <- Store.get s entKey $ liftEitherT . Pkg.fromFile tmp

        say n "Read package description from {}" [entKey]
        Store.copy s entKey ctl dest

        say n "Copyied {} to {}" [build entKey, build dest]
