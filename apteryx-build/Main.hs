{-# LANGUAGE BangPatterns               #-}
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
    , optTemp     :: !FilePath
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

    <*> strOption
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
        <> help ("Maximum number of packages to process concurrently. [default: " ++ show numThreads ++ "]")
        <> value numThreads
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

main :: IO ()
main = do
    Options{..} <- parseOptions options

    n  <- getProgName
    s  <- Store.new optFrom optVersions <$> discoverAWSEnv optDebug

    say n "Looking for entries in {}..." [optFrom]
    xs <- concat <$> Store.entries s

    mapM_ (say n "Discovered {}" . (:[]) . entAnn) xs

    say n "Copying to {}..." [optTo]

    parMapM_ optN (worker s optTemp optTo) xs

    maybe (return ())
          (\a -> say n "Triggering rebuild of {}" [a] >> Index.rebuild a)
          optAddress

    say_ n "Done."
  where
    worker s tmp dest o@Entry{..} = do
        say n "Retrieving {}" [entAnn]
        m <- Store.get o (liftEitherT . Pkg.fromFile tmp) s
        case m of
            Nothing -> say n "Unable to retrieve package from {}" [entAnn]
            Just x  -> do
                say n "Read package description from {}" [entAnn]
                Store.copy o x dest s
                say n "Copied {} to {}" [build entAnn, build dest]
      where
        n = "worker"
