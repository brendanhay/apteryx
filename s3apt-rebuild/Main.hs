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
import           Control.Monad.IO.Class
import           Data.Char             (isDigit)
import           Data.Conduit
import qualified Data.Conduit.Binary   as Conduit
import qualified Data.Conduit.List     as Conduit
import           Data.List             (sort, nub)
import qualified Data.Map.Strict       as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types    (urlEncode)
import           Options.Applicative
import           S3Apt.IO
import           System.Directory
import           System.Exit
import           System.IO

data Arch
    = Amd64
    | I386
    | Other
      deriving (Eq, Ord, Show)

data Entry = Entry
    { entryKey     :: !Text
    , entryName    :: !Text
    , entryVersion :: [Text]
    } deriving (Eq, Show)

instance Ord Entry where
    a `compare` b = f a `compare` f b
      where
        f Entry{..} = Down (entryName, entryVersion)

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
        <> help "S3 bucket to crawl for packages."
         )

    <*> optional (textOption
         $ long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "S3 key prefix to limit the bucket contents to."
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
    ensureExists optIncoming

    r <- enqueue o
    either (\e -> hPrint stderr e >> exitFailure)
           (const $ putStrLn "Completed." >> exitSuccess)
           r

enqueue :: Options -> IO (Either AWSError ())
enqueue o@Options{..} = runAWS AuthDiscover optDebug $ do
    liftIO (putStrLn "Listing bucket contents...")
    xs <- paginate (GetBucket optBucket (Delimiter '/') prefix 200 Nothing)
        $= contents
        $$ catalogue optVersions
    Conduit.sourceList xs
        $= chunked optN
        $= Conduit.mapM (mapM (async . download o))
        $= Conduit.concatMapM (mapM wait)
        $$ Conduit.sinkNull
  where
    prefix = stripPrefix "/" <$> optPrefix

contents :: Monad m => Conduit GetBucketResponse m Contents
contents = Conduit.concatMap (filter match . gbrContents)
  where
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
        Map.insertWith add (arch, key) [Entry bcKey name (digits ver)] m
      where
        add new old = take n . nub . sort $ new <> old

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

download :: Options -> Entry -> AWS ()
download Options{..} Entry{..} = do
    p <- liftIO $ doesFileExist path
    if p
       then liftIO (hPutStrLn stderr $ path ++ " exists.")
        else do
            rs <- send (GetObject optBucket encodedKey [])
            liftIO (putStrLn $ "Downloading " ++ path)
            responseBody rs $$+- Conduit.sinkFile path
  where
    path = Text.unpack $ stripSuffix "/" optIncoming <> "/" <> entryName

    encodedKey = Text.decodeUtf8
        . urlEncode True
        . Text.encodeUtf8
        $ stripPrefix "/" entryKey

chunked :: Monad m => Int -> Conduit a m [a]
chunked n = go []
  where
    go xs = do
        m <- await
        case m of
            Just x | length xs < (n - 1) -> go (x : xs)
            Just x                       -> yield (x : xs) >> go []
            Nothing                      -> void (yield xs)

stripPrefix :: Text -> Text -> Text
stripPrefix x y = fromMaybe y (Text.stripPrefix x y)

stripSuffix :: Text -> Text -> Text
stripSuffix x y = fromMaybe y (Text.stripSuffix x y)
