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
import qualified Data.Text                  as Text
import           Filesystem.Path.CurrentOS
import           Options.Applicative
import           Prelude                    hiding (FilePath)
import           S3Apt.Types
import           System.Directory
import           System.Exit
import           System.IO                  hiding (FilePath)
import           System.Posix.Files
import           System.Process

ensureExists :: FilePath -> IO ()
ensureExists (encodeString -> dir) = do
    createDirectoryIfMissing True dir
    -- Ensure any permission errors are caught.
    p <- doesDirectoryExist dir
    unless p $ hPutStrLn stderr (dir ++ " doesnt exist.") >> exitFailure

parseOptions :: Parser a -> IO a
parseOptions p = customExecParser
    (prefs showHelpOnError)
    (info (helper <*> p) fullDesc)

getFileSize :: MonadIO m => FilePath -> EitherT Error m Size
getFileSize = catchError
    . fmap (fromIntegral . fileSize)
    . getFileStatus
    . encodeString

hashFile :: (MonadIO m, HashAlgorithm a)
         => FilePath
         -> EitherT Error m (Digest a)
hashFile = catchError . Crypto.hashFile . encodeString

withTempFile :: MonadIO m
             => FilePath
             -> String
             -> (FilePath -> Handle -> EitherT Error m a)
             -> EitherT Error m a
withTempFile dir tmpl f = do
    (path, hd) <- catchError $ openTempFile (encodeString dir) tmpl
    f (decodeString path) hd `catchT`
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

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap Text.pack . strOption

pathOption :: Mod OptionFields String -> Parser FilePath
pathOption = fmap decodeString . strOption

debExt :: Text
debExt = ".deb"

catchError :: MonadIO m => IO a -> EitherT Error m a
catchError = EitherT
    . liftIO
    . liftM (fmapL Exception)
    . (try :: IO a -> IO (Either SomeException a))
