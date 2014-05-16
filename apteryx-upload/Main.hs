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

import           Control.Monad.Catch
import           Control.Monad.Trans.Either
import           Data.ByteString            (ByteString)
import qualified Data.Conduit.Binary        as Conduit
import           Data.Monoid
import qualified Filesystem.Path.CurrentOS  as Path
import           Options.Applicative
import           System.APT.IO
import qualified System.APT.Index           as Index
import           System.APT.Log
import           System.APT.Options
import qualified System.APT.Package         as Pkg
import qualified System.APT.Store           as Store
import           System.APT.Types           hiding (urlEncode)
import           System.Environment
import           System.IO

default (ByteString)

data Options = Options
    { optKey     :: !Bucket
    , optFile    :: !Path
    , optTemp    :: !Path
    , optAddress :: Maybe String
    , optDebug   :: !Bool
    } deriving (Eq, Show)

options :: Parser Options
options = Options
    <$> bucketOption
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

    <*> optional (strOption
         $ long "addr"
        <> short 'a'
        <> metavar "ADDR"
        <> help "Server to notify with the new package description. [default: none]"
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

main :: IO ()
main = do
    Options{..} <- parseOptions options
    n           <- getProgName
    e           <- getAWSEnv optDebug

    let path = Path.encodeString optFile

    say n "Reading package description from {}" [path]

    Store.run optKey 1 e $ do
        p <- withFile path ReadMode $ \hd ->
            runEitherT (Pkg.fromFile optTemp $ Conduit.sourceHandle hd)
                >>= either throwM return

        say n "Uploading {} to {}" [build path, build optKey]

        Store.add p optFile

        trigger optAddress p

    say_ n "Done."
  where
    trigger Nothing  _ = return ()
    trigger (Just x) p =
        say "server" "Triggering rebuild of {}" [x] >> Index.reindex p x
