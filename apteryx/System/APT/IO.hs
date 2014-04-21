{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : System.APT.IO
-- Copyright   : (c) 2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module System.APT.IO where

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch        hiding (try)
import           Control.Monad.IO.Class
import           Crypto.Hash
import qualified Crypto.Hash.Conduit        as Crypto
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Filesystem.Path.CurrentOS  as Path
import           System.APT.Types
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

getFileSizeT :: MonadIO m => Path -> EitherT Error m Size
getFileSizeT = catchErrorT
    . fmap (fromIntegral . fileSize)
    . getFileStatus
    . Path.encodeString

hashFileT :: (MonadIO m, HashAlgorithm a)
          => Path
          -> EitherT Error m (Digest a)
hashFileT = catchErrorT . Crypto.hashFile . Path.encodeString

withTempFile :: Path
             -> String
             -> (Path -> Handle -> IO a)
             -> IO a
withTempFile dir tmpl f = runEitherT run >>= either throwM return
  where
    run = withTempFileT dir tmpl (\path hd -> catchErrorT $ f path hd)

withTempFileT :: MonadIO m
              => Path
              -> String
              -> (Path -> Handle -> EitherT Error m a)
              -> EitherT Error m a
withTempFileT dir tmpl f = do
    (path, hd) <- catchErrorT $ openTempFile (Path.encodeString dir) tmpl
    f (Path.decodeString path) hd `catchT`
        \e -> catchErrorT (removeFile path) >> left e

runShellT :: MonadIO m => String -> EitherT Error m ByteString
runShellT cmd = do
    (c, obs, ebs) <- catchErrorT $ createProcess opts >>= run
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

exitEither :: Show e => Either e a -> IO ()
exitEither = either (\ex -> hPrint stderr ex >> exitFailure)
                    (const exitSuccess)

catchErrorT :: MonadIO m => IO a -> EitherT Error m a
catchErrorT = EitherT
    . liftIO
    . liftM (fmapL Exception)
    . (try :: IO a -> IO (Either SomeException a))
