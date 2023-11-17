{-# language KindSignatures      #-}
{-# language RankNTypes          #-}
{-# language ScopedTypeVariables #-}

module System.Nix.Nar.Effects
  ( NarEffects(..)
  , narEffectsIO
  ) where

import Data.Kind ()
import qualified Data.ByteString             as Bytes
import qualified Data.ByteString.Lazy        as Bytes.Lazy
import qualified System.Directory            as Directory
import           System.Posix.Files          ( createSymbolicLink
                                        , fileSize
                                        , getFileStatus
                                        , isDirectory
                                        , readSymbolicLink
                                        )
import qualified System.IO                   as IO
import qualified Control.Monad.IO.Class      as IO
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted    as Exception.Lifted
import qualified Control.Monad.Fail          as MonadFail

data NarEffects (m :: Type -> Type) = NarEffects {
    narReadFile   :: FilePath -> m Bytes.Lazy.ByteString
  , narWriteFile  :: FilePath -> Bytes.Lazy.ByteString -> m ()
  , narStreamFile :: FilePath -> m (Maybe Bytes.ByteString) -> m ()
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
  :: (IO.MonadIO m,
      MonadFail.MonadFail m,
      MonadBaseControl IO m
     ) => NarEffects m
narEffectsIO = NarEffects {
    narReadFile   = IO.liftIO . Bytes.Lazy.readFile
  , narWriteFile  = \a -> IO.liftIO . Bytes.Lazy.writeFile a
  , narStreamFile = streamStringOutIO
  , narListDir    = IO.liftIO . Directory.listDirectory
  , narCreateDir  = IO.liftIO . Directory.createDirectory
  , narCreateLink = \f -> IO.liftIO . createSymbolicLink f
  , narGetPerms   = IO.liftIO . Directory.getPermissions
  , narSetPerms   = \f -> IO.liftIO . Directory.setPermissions f
  , narIsDir      = fmap isDirectory . IO.liftIO . getFileStatus
  , narIsSymLink  = IO.liftIO . Directory.pathIsSymbolicLink
  , narFileSize   = fmap (fromIntegral . fileSize) . IO.liftIO . getFileStatus
  , narReadLink   = IO.liftIO . readSymbolicLink
  , narDeleteDir  = IO.liftIO . Directory.removeDirectoryRecursive
  , narDeleteFile = IO.liftIO . Directory.removeFile
  }


-- | This default implementation for @narStreamFile@ requires @IO.MonadIO@
streamStringOutIO
  :: forall m
  .(IO.MonadIO m,
    MonadFail.MonadFail m,
    MonadBaseControl IO m
  ) => FilePath
  -> m (Maybe Bytes.ByteString)
  -> m ()
streamStringOutIO f getChunk =
  Exception.Lifted.bracket
    (IO.liftIO $ IO.openFile f WriteMode)
    (IO.liftIO . IO.hClose)
    go
  `Exception.Lifted.catch`
    cleanupException
 where
  go :: IO.Handle -> m ()
  go handle = do
    chunk <- getChunk
    case chunk of
      Nothing -> pass
      Just c  -> do
        IO.liftIO $ Bytes.hPut handle c
        go handle
  cleanupException (e :: Exception.Lifted.SomeException) = do
    IO.liftIO $ Directory.removeFile f
    MonadFail.fail $
      "Failed to stream string to " <> f <> ": " <> show e
