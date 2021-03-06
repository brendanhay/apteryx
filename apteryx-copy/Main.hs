{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

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
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.Map.Strict        as Map
import           Data.Monoid            hiding (Any)
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Network.AWS
import           Options.Applicative
import           System.APT.IO
import qualified System.APT.Index       as Index
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package     as Pkg
import           System.APT.Store       (ToKey(..), Store)
import qualified System.APT.Store       as Store
import           System.APT.Types
import           System.Environment
import           System.Exit

default (ByteString)

data Options = Options
    { optFrom     :: !Bucket
    , optTo       :: !Bucket
    , optTemp     :: !FilePath
    , optAddress  :: Maybe String
    , optVersions :: !Int
    , optSemantic :: !Bool
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
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Maximum number of most recent package versions to retain. [default: 3]"
        <> value 3
         )

    <*> switch
         ( long "semantic"
        <> short 's'
        <> help "Whether to use S3 object versions or flat, semantically named keys."
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

data Any where
    AE :: ToKey (Entry a) => Entry a -> Any

instance ToKey Any where
    objectKey b (AE e) = objectKey b e

main :: IO ()
main = do
    Options{..} <- parseOptions options

    n <- getProgName
    e <- getAWSEnv optDebug

    -- FIXME: Check destination bucket has versioning turned on

    say n "Looking for entries in {}..." [optFrom]
    r <- Store.run optVersions e $ do
        xs <- if optSemantic
                  then cat (Store.semantic  optFrom) id
                  else cat (Store.versioned optFrom) Map.elems

        mapM_ (say n "Discovered {}" . Only . objectKey optFrom) (concat xs)

        say n "Copying to {}..." [optTo]
        void $ Store.parMapM (worker optTemp optFrom optTo) xs

    case r of
        Left  x -> say n "Error: {}" (Only $ Shown x) >> exitFailure
        Right _ -> say_ n "Done." >> trigger optAddress
  where
    cat s f = map (map AE . Set.toDescList) . f <$> s

    trigger Nothing  = return ()
    trigger (Just x) =
        say "server" "Triggering rebuild of {}" [x] >> Index.rebuild x

worker :: FilePath -> Bucket -> Bucket -> [Any] -> Store ()
worker tmp bf bt xs = mapM_ (\x -> tid >>= go x) xs
  where
    go kf n = do
        let k = objectKey bf kf

        say n "Downloading {}..." [k]
        m <- Store.get bf kf (liftEitherT . Pkg.fromFile tmp)

        case m of
            Nothing -> say n "Unable to retrieve package from {}" [k]
            Just kt -> do
                say n "Retrieved package description from {}" [k]
                r <- Store.monotonic bt kt (Store.copy bf kf bt kt)

                maybe (say n "{} already exists, skipping" [k])
                      (const $ say n "Copied {} from {}" [build k, build bf])
                      r

    tid = (Text.drop 9 . Text.pack . show) `liftM` liftIO myThreadId
