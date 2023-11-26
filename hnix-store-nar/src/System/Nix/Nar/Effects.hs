{-# LANGUAGE RankNTypes #-}

module System.Nix.Nar.Effects
  ( NarEffects(..)
  , narEffectsIO
  , IsExecutable(..)
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
import Data.Bool (bool)
import Data.Int (Int64)
import Data.Kind (Type)
import System.IO (Handle, IOMode(WriteMode))

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.ByteString.Lazy        as Bytes.Lazy
import qualified System.Directory            as Directory
import           System.Posix.Files          ( createSymbolicLink
                                             , fileSize
                                             , getFileStatus
                                             , isDirectory
                                             , readSymbolicLink
                                             )
import qualified System.IO                   as IO
import qualified Control.Exception.Lifted    as Exception.Lifted

data IsExecutable = NonExecutable | Executable
  deriving (Eq, Show)

data NarEffects (m :: Type -> Type) = NarEffects {
    narReadFile   :: FilePath -> m Bytes.Lazy.ByteString
  , narWriteFile  :: FilePath -> IsExecutable -> Bytes.Lazy.ByteString -> m ()
  , narStreamFile :: FilePath -> IsExecutable -> m (Maybe ByteString) -> m ()
  , narListDir    :: FilePath -> m [FilePath]
  , narCreateDir  :: FilePath -> m ()
  , narCreateLink :: FilePath -> FilePath -> m ()
  , narIsExec     :: FilePath -> m IsExecutable
  , narIsDir      :: FilePath -> m Bool
  , narIsSymLink  :: FilePath -> m Bool
  , narFileSize   :: FilePath -> m Int64
  , narReadLink   :: FilePath -> m FilePath
  , narDeleteDir  :: FilePath -> m ()
  , narDeleteFile :: FilePath -> m ()
}

-- | A particular @NarEffects@ that uses regular POSIX for file manipulation
--   You would replace this with your own @NarEffects@ if you wanted a
--   different backend
narEffectsIO
  :: ( MonadIO m
     , MonadFail m
     , MonadBaseControl IO m
     )
  => NarEffects m
narEffectsIO = NarEffects {
    narReadFile   = liftIO . Bytes.Lazy.readFile
  , narWriteFile  = \f e c -> liftIO $ do
      Bytes.Lazy.writeFile f c
      p <- Directory.getPermissions f
      Directory.setPermissions f (p { Directory.executable = e == Executable })
  , narStreamFile = streamStringOutIO
  , narListDir    = liftIO . Directory.listDirectory
  , narCreateDir  = liftIO . Directory.createDirectory
  , narCreateLink = \f -> liftIO . createSymbolicLink f
  , narIsExec     = liftIO . (fmap (bool NonExecutable Executable . Directory.executable)) . Directory.getPermissions
  , narIsDir      = fmap isDirectory . liftIO . getFileStatus
  , narIsSymLink  = liftIO . Directory.pathIsSymbolicLink
  , narFileSize   = fmap (fromIntegral . fileSize) . liftIO . getFileStatus
  , narReadLink   = liftIO . readSymbolicLink
  , narDeleteDir  = liftIO . Directory.removeDirectoryRecursive
  , narDeleteFile = liftIO . Directory.removeFile
  }

-- | This default implementation for @narStreamFile@ requires @MonadIO@
streamStringOutIO
  :: forall m
   . ( MonadIO m
     , MonadFail m
     , MonadBaseControl IO m
     )
  => FilePath
  -> IsExecutable
  -> m (Maybe ByteString)
  -> m ()
streamStringOutIO f executable getChunk =
  Exception.Lifted.bracket
    (liftIO $ IO.openFile f WriteMode)
    (\h -> liftIO (updateExecutablePermissions >> IO.hClose h))
    go
  `Exception.Lifted.catch`
    cleanupException
 where
  go :: Handle -> m ()
  go handle = do
    chunk <- getChunk
    case chunk of
      Nothing -> pure ()
      Just c  -> do
        liftIO $ Data.ByteString.hPut handle c
        go handle
  updateExecutablePermissions =
    Control.Monad.when (executable == Executable) $ do
      p <- Directory.getPermissions f
      Directory.setPermissions f (p { Directory.executable = True })
  cleanupException (e :: Exception.Lifted.SomeException) = do
    liftIO $ Directory.removeFile f
    Control.Monad.fail $
      "Failed to stream string to " <> f <> ": " <> show e
