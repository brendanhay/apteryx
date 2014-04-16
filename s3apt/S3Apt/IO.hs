{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : S3Apt.IO
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module S3Apt.IO where

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Crypto.Hash
import qualified Crypto.Hash.Conduit        as Crypto
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  (Text)
import qualified Filesystem.Path.CurrentOS  as Path
import           S3Apt.Log
import           S3Apt.Types
import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix.Files
import           System.Process

ensureExists :: Path -> IO ()
ensureExists (Path.encodeString -> dir) = do
    createDirectoryIfMissing True dir
    -- Ensure any permission errors are caught.
    p <- doesDirectoryExist dir
    unless p $ hPutStrLn stderr (dir ++ " doesnt exist.") >> exitFailure

getFileSize :: MonadIO m => Path -> EitherT Error m Size
getFileSize = catchError
    . fmap (fromIntegral . fileSize)
    . getFileStatus
    . Path.encodeString

hashFile :: (MonadIO m, HashAlgorithm a)
         => Path
         -> EitherT Error m (Digest a)
hashFile = catchError . Crypto.hashFile . Path.encodeString

withTempFile :: MonadIO m
             => Path
             -> String
             -> (Path -> Handle -> EitherT Error m a)
             -> EitherT Error m a
withTempFile dir tmpl f = do
    (path, hd) <- catchError $ openTempFile (Path.encodeString dir) tmpl
    f (Path.decodeString path) hd `catchT`
        \e -> catchError (removeFile path) >> left e

runShell :: MonadIO m => String -> EitherT Error m ByteString
runShell cmd = do
    (c, obs, ebs) <- catchError $ createProcess opts >>= run
    case c of
        ExitFailure n -> left  $ ShellError n cmd ebs
        ExitSuccess   -> right $ LBS.toStrict obs
  where
    run (Just inh, Just outh, Just errh, hd) = do
        obs <- LBS.hGetContents outh
        ebs <- LBS.hGetContents errh
        hClose inh
        a <- async $ evaluate (rnf obs) >> hClose outh
        b <- async $ evaluate (rnf ebs) >> hClose errh
        void $ waitEitherCancel a b
        c <- waitForProcess hd
        return (c, obs, ebs)

    run _ = return (ExitFailure 1, "", "")

    opts = (shell cmd)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

runMain :: Show e => Text -> IO (Either e a) -> IO ()
runMain name m = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    say_ name "Starting..."
    exit <- m
    either (\ex -> hPrint stderr ex >> exitFailure)
           (const $ say_ name "Completed." >> exitSuccess)
           exit

catchError :: MonadIO m => IO a -> EitherT Error m a
catchError = EitherT
    . liftIO
    . liftM (fmapL Exception)
    . (try :: IO a -> IO (Either SomeException a))
