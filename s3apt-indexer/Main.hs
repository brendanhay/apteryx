{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

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
import           Control.Concurrent.Async         as Async
import           Control.Concurrent.STM
import           Control.DeepSeq                  (rnf)
import           Control.Error
import qualified Control.Exception                as Ex
import           Control.Monad
import           Crypto.Hash
import           Crypto.Hash.Conduit
import           Data.Attoparsec.ByteString.Char8 (char, anyChar, space)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Attoparsec.Combinator       hiding (option)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Base64           as Base64
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Byteable
import           Data.CaseInsensitive             (mk)
import           Data.Char                        (isAlpha)
import qualified Data.Conduit.Binary              as Conduit
import           Data.List                        (isSuffixOf)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Typeable
import           Filesystem.Path.CurrentOS
import           Network.AWS.S3
import           Network.HTTP.Conduit
import           Options.Applicative
import           Prelude                          hiding (FilePath)
import           S3Apt.IO
import           S3Apt.Types
import           System.Directory
import           System.Exit
import           System.FSNotify
import           System.IO                        hiding (FilePath)
import           System.Posix.Files
import           System.Posix.Signals
import           System.Process

data Options = Options
    { optBucket   :: !Text
    , optPrefix   :: Maybe Text
    , optIncoming :: !FilePath
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
        <> help "S3 bucket to store packages in."
         )

    <*> optional (textOption
         $ long "prefix"
        <> short 'p'
        <> metavar "PREFIX"
        <> help "Additional prefix for S3 keys."
         )

    <*> pathOption
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
        <> help "Maximum number of concurrent downloads. default: 4"
        <> value 4
         )

    <*> option
         ( long "versions"
        <> short 'v'
        <> metavar "INT"
        <> help "Number versions to allow in the package index. default: 3"
        <> value 3
         )

    <*> switch
         ( long "debug"
        <> short 'd'
        <> help "Print debug output."
         )

data Task
    = Exit
    | Task FilePath


-- FIXME: watcher picks up creation of optIncoming
main :: IO ()
main = do
    o@Options{..} <- parseOptions options

    setBuffering
    ensureExists optIncoming

    let dir = encodeString optIncoming
        ext = Text.unpack debExt

    -- Work queue
    q  <- atomically $ newTBQueue optN

    -- Start workers
    ws <- mapM (const . Async.async $ worker o q) [1..optN]

    -- Watch incoming dir for added files
    m  <- startManager
    watchTree m optIncoming added (watch q)

    -- Populate the work queue with an existing .debs
    filter (isSuffixOf ext) <$> getDirectoryContents dir >>=
        mapM_ (enqueue optIncoming q)

    -- Exit gracefully on signal
    void $ installHandler keyboardSignal (shutdown q optN) Nothing

    -- Wait for all workers to exit
    (w, e) <- Async.waitAnyCatch ws

    either (print . ("Worker exited with: " ++) . show)
           (const $ return ())
           e

    shutdown q (length ws - 1)

    mapM_ Async.wait (filter (/= w) ws)

    stopManager m
  where
    added (Added _ _) = True
    added _           = False

    watch q (Added p _) = atomically . writeTBQueue q $ Task p
    watch _ _           = return ()

    enqueue dir q = atomically
        . writeTBQueue q
        . Task
        . (dir </>)
        . decodeString

    shutdown q n = Catch
        . atomically
        $ mapM_ (const $ writeTBQueue q Exit) [1..n]

worker :: Options -> TBQueue Task -> IO ()
worker opts queue = do
    f <- msg . drop 9 . show <$> myThreadId
    f "started."
    go f
  where
    msg t s = putStrLn $ "Worker " ++ t ++ " " ++ s

    go f = do
        x <- atomically $ readTBQueue queue
        case x of
            Exit   -> f "exiting..."
            Task p -> do
                _ <- f ("processing " ++ show p)
                runScript (loadControl opts p) >>= print
                go f

newtype Size = Size Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

instance Byteable Size where
    toBytes (Size n) = BS.pack (show n)

data Control = Control
    { pkgControl  :: !ByteString
    , pkgVersion  :: !ByteString
    , pkgArch     :: !Arch
    , pkgSize     :: !Size
    , pkgMD5Sum   :: !(Digest MD5)
    , pkgSHA1     :: !(Digest SHA1)
    , pkgSHA256   :: !(Digest SHA256)
    , pkgOptional :: Map ByteString ByteString
    } deriving (Eq, Show)

loadControl :: Options -> FilePath -> Script Control
loadControl Options{..} path =
    hoistError =<< parseControl
        <$> getControlFields path
        <*> getFileSize path
        <*> hashPackage path
        <*> hashPackage path
        <*> hashPackage path

--    runAWS AuthDiscover optDebug $ uploadControl o ctrl path

getControlFields :: FilePath -> Script (Map ByteString ByteString)
getControlFields (encodeString -> path) = do
    bs <- runShell ("ar -p " ++ path ++ " control.tar.gz | tar -Ox control")
    either left (right . Map.fromList) (P.parseOnly fields bs)
  where
    fields = many' ((,) <$> key <*> val)

    key = P.takeWhile1 isAlpha <* char ':' <* many1 space
    val = BS.pack <$> manyTill anyChar continue

    continue = do
        p <- choice
            [ P.string "\n " >> return False
            , P.endOfLine >> return True
            , endOfInput >> return True
            ]
        if p
           then return ()
           else fail ""

parseControl :: Map ByteString ByteString
             -> Size
             -> Digest MD5
             -> Digest SHA1
             -> Digest SHA256
             -> Either String Control
parseControl fs size md5 sha1 sha256 = Control
    <$> require "Package"
    <*> require "Version"
    <*> (archFromBS <$> require "Architecture")
    <*> pure size
    <*> pure md5
    <*> pure sha1
    <*> pure sha256
    <*> pure fields
  where
    require k = note
        (BS.unpack $ "Unable to find " <> k <> " in package description")
        (Map.lookup k fs)

    fields = Map.map (BS.take 1024)
        $ Map.filterWithKey (\k -> const $ mk k `notElem` reserved) fs

    reserved =
        [ "Package"
        , "Version"
        , "Architecture"
        , "Filename"
        , "Size"
        , "MD5Sum"
        , "SHA1"
        , "SHA256"
        ]

hashPackage :: HashAlgorithm a => FilePath -> Script (Digest a)
hashPackage = scriptIO . hashFile . encodeString

getFileSize :: FilePath -> Script Size
getFileSize = scriptIO
     . fmap (fromIntegral .  fileSize)
     . getFileStatus
     . encodeString

uploadPackage :: Options -> Control -> FilePath -> AWS ()
uploadPackage Options{..} Control{..} path =
    send_ PutObject
        { poBucket  = optBucket
        , poKey     = Text.pack $ encodeString (filename path)
        , poHeaders = headers
        , poBody    = body
        }
  where
    headers = ("Content-MD5", enc pkgMD5Sum) :
        [ "Control"      =@ pkgControl
        , "Version"      =@ pkgVersion
        , "Architecture" =@ toBytes pkgArch
        , "Size"         =@ toBytes pkgSize
        , "SHA1"         =@ enc pkgSHA1
        , "SHA256"       =@ enc pkgSHA256
        ]

    (=@) k = ("x-amz-meta-" <> k,)

    body = requestBodySource (fromIntegral pkgSize) $
        Conduit.sourceFile (encodeString path)

    enc :: Byteable a => a -> ByteString
    enc = Base64.encode . toBytes

data ShellException = ShellException String
    deriving (Show, Typeable)

instance Ex.Exception ShellException

runShell :: String -> Script ByteString
runShell cmd = scriptIO $ createProcess opts >>=
    \(Just inh, Just outh, Just errh, hd) -> do
        obs <- LBS.hGetContents outh
        ebs <- LBS.hGetContents errh

        hClose inh

        a <- Async.async $ Ex.evaluate (rnf obs) >> hClose outh
        b <- Async.async $ Ex.evaluate (rnf ebs) >> hClose errh

        void $ waitEitherCancel a b

        c <- waitForProcess hd
        case c of
            ExitFailure _ -> Ex.throwIO (ShellException cmd)
            ExitSuccess   -> return (LBS.toStrict obs)
  where
    opts = (shell cmd)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
