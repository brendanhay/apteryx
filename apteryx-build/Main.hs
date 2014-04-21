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
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Morph
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Network.APT.S3        as S3
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Options.Applicative
import           System.APT.IO
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package    as Pkg
import           System.APT.Types
import           System.Environment    hiding (getEnv)

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
    name          <- BS.pack <$> getProgName
    lgr           <- newLogger

    res <- runAWS AuthDiscover optDebug $ do
        xs  <- S3.entries lgr name optFrom optVersions
        env <- getEnv
        liftIO $ parForM optN (concat xs)
            (runEnv env . build lgr o)
            (const $ return ())

    exitEither res

build :: Logger -> Options -> Entry -> AWS ()
build lgr Options{..} Entry{..} = do
    say lgr name "Retrieving {}" [entKey]
    rs   <- send $ GetObject (keyBucket entKey) (keyPrefix entKey) []
    (bdy, f) <- unwrapResumable (responseBody rs)

    say lgr name "Parsing control from {}" [entKey]
    env  <- getEnv
    ctl  <- liftEitherT (Pkg.fromFile optTemp (aws env bdy)) `finally` f

    code <- status <$> S3.copy lgr name entKey ctl optTo
    say lgr name "Completed {}" [code]
  where
    status = Text.pack . show . statusCode . responseStatus

    name = Text.encodeUtf8
        . Text.drop 1
        $ Text.dropWhile (/= '/') (keyPrefix entKey)

    aws e = hoist $ either throwM return <=< runEnv e
