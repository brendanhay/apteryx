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
import           Control.Concurrent.Async  (waitEitherCancel)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char                 (isDigit)
import           Data.Conduit
import qualified Data.Conduit.Binary       as Conduit
import qualified Data.Conduit.List         as Conduit
import           Data.List                 (sort, nub)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Encoding        as Text
import           Data.Word
import           Filesystem.Path.CurrentOS hiding (stripPrefix, concat)
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Network.HTTP.Types        (urlEncode)
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           S3Apt.IO
import           S3Apt.Rebuild.Options
import           S3Apt.Types
import           System.Directory
import           System.Exit
import           System.IO                 hiding (FilePath)

data Entry = Entry
    { entryKey     :: !Text
    , entryName    :: !Text
    , entryVersion :: [Text]
    } deriving (Eq, Show)

instance Ord Entry where
    a `compare` b = f a `compare` f b
      where
        f Entry{..} = Down (entryName, entryVersion)

main :: IO ()
main = do
--    setBuffering
    o@Options{..} <- parseOptions options

    return ()
--    d <- download o
    -- u <- enqueue o

    -- void $ waitEitherCancel d u

    -- either (\e -> hPrint stderr e >> exitFailure)
    --        (const $ putStrLn "Completed." >> exitSuccess)
    --        r

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

-- process Options{..} Entry{..} man = do
--     rs <- send (GetObject optBucket encodedKey [])
--     responseBody rs $$+- Conduit.sinkFile path

--     liftIO (putStrLn $ "Downloading " ++ path)
--     rs <- send $ GetObject optBucket encodedKey []

--     requestBodySourceChunkedIO (responseBody rs)

--     request <- parseUrl "http://google.com/"
--       withManager $ \manager -> do
--           response <- http request manager
--           responseBody response C.$$+- sinkFile "google.html"
--   where

--     encodedKey = Text.decodeUtf8
--         . urlEncode True
--         . Text.encodeUtf8
--         $ stripPrefix "/" entryKey

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
    path = encodeString $ optIncoming </> fromText entryName

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
