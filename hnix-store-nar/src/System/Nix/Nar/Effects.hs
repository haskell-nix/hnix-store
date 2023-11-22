{-# LANGUAGE RankNTypes #-}

module System.Nix.Nar.Effects
  ( NarEffects(..)
  , narEffectsIO
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.ByteString (ByteString)
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

data NarEffects (m :: Type -> Type) = NarEffects {
    narReadFile   :: FilePath -> m Bytes.Lazy.ByteString
  , narWriteFile  :: FilePath -> Bytes.Lazy.ByteString -> m ()
  , narStreamFile :: FilePath -> m (Maybe ByteString) -> m ()
  , narListDir    :: FilePath -> m [FilePath]
  , narCreateDir  :: FilePath -> m ()
  , narCreateLink :: FilePath -> FilePath -> m ()
  , narGetPerms   :: FilePath -> m Directory.Permissions
  , narSetPerms   :: FilePath -> Directory.Permissions ->  m ()
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
  , narWriteFile  = \a -> liftIO . Bytes.Lazy.writeFile a
  , narStreamFile = streamStringOutIO
  , narListDir    = liftIO . Directory.listDirectory
  , narCreateDir  = liftIO . Directory.createDirectory
  , narCreateLink = \f -> liftIO . createSymbolicLink f
  , narGetPerms   = liftIO . Directory.getPermissions
  , narSetPerms   = \f -> liftIO . Directory.setPermissions f
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
  -> m (Maybe ByteString)
  -> m ()
streamStringOutIO f getChunk =
  Exception.Lifted.bracket
    (liftIO $ IO.openFile f WriteMode)
    (liftIO . IO.hClose)
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
  cleanupException (e :: Exception.Lifted.SomeException) = do
    liftIO $ Directory.removeFile f
    Control.Monad.fail $
      "Failed to stream string to " <> f <> ": " <> show e
