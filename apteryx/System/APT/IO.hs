{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch        hiding (try, finally)
import           Control.Monad.IO.Class
import qualified Crypto.Hash.Conduit        as Crypto
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Filesystem.Path.CurrentOS  as Path
import           Network.AWS                hiding (async)
import           System.APT.Types
import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix.Files
import           System.Process

default (ByteString)

ensureExists :: MonadIO m => Path -> m ()
ensureExists (Path.encodeString -> dir) = liftIO $ do
    createDirectoryIfMissing True dir
    -- Ensure any permission errors are caught.
    p <- doesDirectoryExist dir
    unless p $ hPutStrLn stderr (dir ++ " doesnt exist.") >> exitFailure

-- getFileSize :: MonadIO m => Path -> EitherT Error m Size
-- getFileSize = catchErrorT
--     . fmap (fromIntegral . fileSize)
--     . getFileStatus
--     . Path.encodeString

getFileStat :: MonadIO m => Path -> m Stat
getFileStat (Path.encodeString -> path) = liftIO $ Stat
    <$> (fromIntegral . fileSize <$> getFileStatus path)
    <*> Crypto.hashFile path
    <*> Crypto.hashFile path
    <*> Crypto.hashFile path

withTempFile :: Path
             -> String
             -> (Path -> Handle -> IO a)
             -> IO a
withTempFile dir tmpl f = do
    (path, hd) <- openTempFile (Path.encodeString dir) tmpl
    f (Path.decodeString path) hd `finally` (hClose hd >> removeFile path)

withTempFileT :: MonadIO m
              => Path
              -> String
              -> (Path -> Handle -> EitherT Error IO a)
              -> EitherT Error m a
withTempFileT dir tmpl f =
    catchErrorT (withTempFile dir tmpl $ \path hd -> runEitherT (f path hd))
        >>= hoistEither

runShell :: MonadIO m => String -> EitherT Error m ByteString
runShell cmd = do
    (c, obs, ebs) <- catchErrorT $ createProcess opts >>= run
    case c of
        ExitFailure _ -> left  (shellError $ LBS.toStrict ebs)
        ExitSuccess   -> right (LBS.toStrict obs)
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

loadEnv :: (MonadThrow m, MonadIO m) => Bool -> m AWSEnv
loadEnv dbg = liftIO $
    runEitherT (loadAWSEnv AuthDiscover dbg)
        >>= either (throwM . awsError) return

catchErrorT :: MonadIO m => IO a -> EitherT Error m a
catchErrorT = EitherT
    . liftIO
    . liftM (fmapL Exception)
    . (try :: IO a -> IO (Either SomeException a))
