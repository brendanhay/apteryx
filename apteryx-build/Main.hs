
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

-- FIXME: apt-cacher-ng behaviour

-- FIXME:
--   Add verification to the build process
--   Ensure number of versions is correct, metadata can be loaded
--   Switch to using HasLogger, LogT monad stack

-- http://www.deb-multimedia.org/dists/unstable/main/binary-amd64/Packages

-- dist/{stable,unstable,testing}/{main,contrib,non-free}/{binary-all,binary-amd64}

-- Add components as a csv list to x-amz-components
-- Add components as flags to upload, build? intelligently copy component meta, or override

-- FIXME:
-- need to generate/serve a Realease file?
--
-- ability to specify components when uploading? should affect prefix?
--   correpkgy bucket/separate components in storage etc.
--
-- generate correct filename in Packages/.gz
--
-- add handler to redirect/301 to correct S3 file based on filename
--
-- repository signing
--
-- tidy up the triggering of successful reindex/rebuild

-- How to do an incremental rebuild? Not possible without keeping all package meta
-- in memory?

-- Would solve the issue of streaming to file etc - just get all keys, keep result
-- in memory, and write to disk/gzip everytime a PATCH is received.

-- On rebuild, try to rebuild it and if succeeds throw away and replace in memory.

-- Pooled repository? - Maybe not much point since non-cross compiled binaries
-- deb uri distribution [component1] [component2] [...]
-- Archive
-- The name of the distribution of Debian the packages in this directory belong to (or are designed for), i.e. stable, testing or unstable.
-- Component
-- The component of the packages in the directory, for example main, non-free, or contrib.
-- Origin
-- The name of who made the packages.
-- Label
-- Some label adequate for the packages or for your repository. Use your fantasy.
-- Architecture
-- The architecture of the packages in this directory, such as i386, sparc or source.
-- It is important to get Archive and Architecture right, as they're most used for pinning. The others are less important.

main :: IO ()
main = do
    Options{..} <- parseOptions options

    n  <- getProgName
    s  <- Store.new optFrom optVersions <$> loadEnv optDebug

    say n "Looking for entries in {}..." [optFrom]
    xs <- concat <$> Store.entries s

    say n "Discovered {} packages." [length xs]
    mapM_ (say n "Found {}" . (:[]) . entAnn) xs

--    say n "Copying to {}..." [optTo]
  --   parForM optN (worker s optTemp optTo) (const $ return ()) xs

  --   maybe (return ())
  --         (\a -> say n "Triggering rebuild of {}" [a] >> Index.rebuild a)
  --         optAddress

  --   say_ n "Done."
  -- where
  --   worker s tmp dest o@Entry{..} = do
  --       n <- (mappend "worker-") . drop 9 . show <$> myThreadId

  --       say n "Retrieving {}" [entAnn]
  --       m <- Store.get o (liftEitherT . Pkg.fromFile tmp) s

  --       maybe (say n "Unable to retrieve package from {}" [entAnn])
  --             (\x -> do
  --                 say n "Read package description from {}" [entAnn]
  --                 Store.copy o x dest s
  --                 say n "Copied {} to {}" [build entAnn, build dest])
  --             m

