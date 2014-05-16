{-# LANGUAGE BangPatterns               #-}
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
import qualified Control.Concurrent.Async     as Async
import           Control.DeepSeq
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch          hiding (try, finally)
import           Control.Monad.IO.Class
import qualified Control.Monad.Par.Combinator as Par
import qualified Control.Monad.Par.IO         as Par
import qualified Crypto.Hash.Conduit          as Crypto
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy.Char8   as LBS
import           Data.Function
import           Data.List                    (deleteFirstsBy)
import           Network.AWS                  hiding (async)
import           System.APT.Types
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import qualified System.IO.Temp               as Temp
import           System.Posix.Files
import           System.Process

default (ByteString)

getFileStat :: MonadIO m => FilePath -> m Stat
getFileStat path = liftIO $ Stat
    <$> (fromIntegral . fileSize <$> getFileStatus path)
    <*> Crypto.hashFile path
    <*> Crypto.hashFile path
    <*> Crypto.hashFile path

withTempFile :: MonadIO m
             => FilePath
             -> String
             -> (FilePath -> Handle -> EitherT Error IO a)
             -> EitherT Error m a
withTempFile dir tmpl f =
    catchError (Temp.withTempFile dir tmpl $ \x -> runEitherT . f x)
        >>= hoistEither

runShell :: MonadIO m => String -> EitherT Error m ByteString
runShell cmd = do
    (c, obs, ebs) <- catchError $ createProcess opts >>= run
    case c of
        ExitFailure _ -> left  (shellError $ LBS.toStrict ebs)
        ExitSuccess   -> right (LBS.toStrict obs)
  where
    run (Just inh, Just outh, Just errh, hd) = do
        obs <- LBS.hGetContents outh
        ebs <- LBS.hGetContents errh

        hClose inh

        a <- Async.async $ evaluate (rnf obs) >> hClose outh
        b <- Async.async $ evaluate (rnf ebs) >> hClose errh

        void $ Async.waitEitherCancel a b

        c <- waitForProcess hd

        return (c, obs, ebs)

    run _ = return (ExitFailure 1, "", "")

    opts = (shell cmd)
        { std_in  = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

catchError :: MonadIO m => IO a -> EitherT Error m a
catchError = EitherT
    . liftIO
    . liftM (fmapL Exception)
    . (try :: IO a -> IO (Either SomeException a))

diff :: (Functor m, MonadIO m) => FilePath -> FilePath -> m [Path]
diff !a !b = liftM2 f (listFiles b) (listFiles a)
  where
    f xs ys = deleteFirstsBy ((==) `on` relative) xs ys

listFiles :: (MonadIO m, Functor m) => FilePath -> m [Path]
listFiles !d = foldFiles d
  where
    foldFiles !f = do
        c <- liftIO $ (,,)
            <$> (readable <$> getPermissions f)
            <*> doesFileExist f
            <*> doesDirectoryExist f
        case c of
            (True, True, False) ->
                return [F f (makeRelative d f)]
            (True, False, True) ->
                (D f (makeRelative d f) :) . concat <$> foldDir f
            _                   ->
                return []

    foldDir f =
        liftIO (getDirectoryContents f) >>=
            mapM foldFiles . map (f </>) . filter dots

    dots "."  = False
    dots ".." = False
    dots _    = True

removePath :: Path -> IO ()
removePath p = exist f >>= (`when` rm f)
  where
    (exist, rm) = case p of
        F{} -> (doesFileExist, removeFile)
        D{} -> (doesDirectoryExist, removeDirectoryRecursive)

    f = absolute p

parMapM :: (MonadIO m, NFData b) => (a -> Par.ParIO b) -> [a] -> m [b]
parMapM f = liftIO . Par.runParIO . Par.parMapM f

getAWSEnv :: Bool -> IO AWSEnv
getAWSEnv d = runEitherT (loadAWSEnv AuthDiscover d) >>=
    either (throwM . awsError) return
