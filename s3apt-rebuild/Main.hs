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
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Morph
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isDigit)
import           Data.Conduit
import qualified Data.Conduit.List     as Conduit
import           Data.Int
import           Data.List             (sort, nub)
import qualified Data.Map.Strict       as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Data.Text.Format      (Shown(..))
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           S3Apt.IO
import           S3Apt.Log
import           S3Apt.Types
import           System.Exit
import           System.IO

data Entry = Entry
    { entryKey     :: !Text
    , entryName    :: !Text
    , entryVersion :: [Text]
    , entrySize    :: !Int64
    } deriving (Eq, Show)

instance Ord Entry where
    a `compare` b = f a `compare` f b
      where
        f Entry{..} = Down (entryName, entryVersion)

data Options = Options
    { optBucket   :: !Text
    , optPrefix   :: Maybe Text
    , optAddress  :: !Text
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
        <> help "S3 bucket to crawl for packages."
         )

    <*> optional (textOption
         $ long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "S3 key prefix to limit the bucket contents to. default: ''"
         )

    <*> textOption
         ( long "addr"
        <> short 'a'
        <> metavar "ADDR"
        <> help "Address to upload packages. default: http://localhost:8080/packages"
        <> value "http://localhost:8080/packages"
         )

    <*> option
         ( long "concurrency"
        <> short 'c'
        <> metavar "INT"
        <> help "Maximum number of concurrent downloads. default: 10"
        <> value 10
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Number versions to limit the downloads to. default: 3"
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

    setBuffering
    say_ name "Starting..."

    man <- newManager conduitManagerSettings
    rq  <- parseUrl (Text.unpack optAddress)

    let chk = rq { method = "HEAD", path = "/i/status" }
    say name "Checking status of http://{}:{}{}"
        [BS.unpack $ host chk, show $ port chk, BS.unpack $ path chk]
    void $ httpLbs chk man

    rs  <- runAWS AuthDiscover optDebug $ do
        let pre = stripPrefix "/" <$> optPrefix
        env <- getEnv

        say name "Paginating contents of {}" [optBucket]
        xs  <- paginate (GetBucket optBucket (Delimiter '/') pre 200 Nothing)
            $= Conduit.concatMap (filter match . gbrContents)
            $$ catalogue optVersions

        say name "Uploading files to {}" [optAddress]
        Conduit.sourceList xs
            $= chunked optN
            $= Conduit.mapM (mapM (async . forward o env man rq))
            $= Conduit.concatMapM (mapM wait)
            $$ Conduit.sinkNull

    either (\ex -> hPrint stderr ex >> exitFailure)
           (const $ say_ name "Completed." >> exitSuccess)
           rs
  where
    name = "main" :: Text

    match Contents{..}
        | bcSize == 0               = False
        | bcStorageClass == Glacier = False
        | otherwise                 = debExt `Text.isSuffixOf` bcKey

-- FIXME: pulls all the keys into memory
catalogue :: Monad m => Int -> Consumer Contents m [Entry]
catalogue n = go mempty
  where
    go m = await >>= maybe (return . concat $ Map.elems m) (go . entry m)

    entry m Contents{..} =
        Map.insertWith add (arch, key) [Entry bcKey name (digits ver) size] m
      where
        add new old = take n . nub . sort $ new <> old

        size = fromIntegral bcSize

        (arch, ver)
            | Just x <- "amd64" `Text.stripSuffix` suf = (Amd64, x)
            | Just x <- "i386"  `Text.stripSuffix` suf = (I386,  x)
            | otherwise                                = (Other, suf)

        (key, suf) = Text.break isDigit $ stripSuffix debExt name

        name = last $ Text.split (== '/') bcKey

        digits = filter (not . Text.null)
            . map (Text.filter isDigit)
            . Text.split delim

        delim = flip elem "_.\\+~"

chunked :: Monad m => Int -> Conduit a m [a]
chunked n = go []
  where
    go xs = do
        m <- await
        case m of
            Just x | length xs < (n - 1) -> go (x : xs)
            Just x                       -> yield (x : xs) >> go []
            Nothing                      -> void (yield xs)

forward :: Options -> Env -> Manager -> Request -> Entry -> AWS ()
forward Options{..} env man rq Entry{..} = do
    say logKey "Retrieving from {}" [optBucket]
    (bdy, f) <- send (GetObject optBucket encodedKey []) >>=
        unwrapResumable . responseBody

    let fwd = rq { requestBody = requestBodySourceIO entrySize (hoist aws bdy)
                 , method      = "POST"
                 }

    say logKey "Forwarding to {}" [optAddress]
    rs <- http fwd man `finally` f

    say logKey "Status {}" [status rs]
    responseBody rs $$+- return ()
  where
    aws = either throwM return <=< (`runEnv` env)

    status = Text.pack . show . statusCode . responseStatus
    logKey = Text.drop 1 $ Text.dropWhile (/= '/') entryKey

    encodedKey = Text.decodeUtf8
        . urlEncode True
        . Text.encodeUtf8
        $ stripPrefix "/" entryKey
