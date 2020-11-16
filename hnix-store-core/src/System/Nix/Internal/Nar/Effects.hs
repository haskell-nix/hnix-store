{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Nix.Internal.Nar.Effects
  ( NarEffects(..)
  , PathType(..)
  , narEffectsIO
  ) where

import qualified Control.Exception.Lifted    as Lifted
import qualified Control.Monad.Fail          as MonadFail
import qualified Control.Monad.IO.Class      as IO
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Int                    (Int64)
import qualified System.Directory            as Directory
import qualified System.Directory            as Directory
import qualified System.IO                   as IO
import           System.Posix.Files          (createSymbolicLink, fileSize, readSymbolicLink,
                                              getSymbolicLinkStatus, isRegularFile, isDirectory, isSymbolicLink)

data PathType = Regular | Directory | Symlink | Unknown deriving Show

pathTypeFromPosix status
  | isRegularFile  status = Regular
  | isDirectory    status = Directory
  | isSymbolicLink status = Symlink
  | otherwise             = Unknown

data NarEffects (m :: * -> *) = NarEffects {
    narReadFile   :: FilePath -> m BSL.ByteString
  , narWriteFile  :: FilePath -> BSL.ByteString -> m ()
  , narStreamFile :: FilePath -> m (Maybe BS.ByteString) -> m ()
  , narListDir    :: FilePath -> m [FilePath]
  , narCreateDir  :: FilePath -> m ()
  , narCreateLink :: FilePath -> FilePath -> m ()
  , narGetPerms   :: FilePath -> m Directory.Permissions
  , narSetPerms   :: FilePath -> Directory.Permissions ->  m ()
  , narPathType   :: FilePath -> m PathType
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
    narReadFile   = IO.liftIO . BSL.readFile
  , narWriteFile  = \a b -> IO.liftIO $ BSL.writeFile a b
  , narStreamFile = streamStringOutIO
  , narListDir    = IO.liftIO . Directory.listDirectory
  , narCreateDir  = IO.liftIO . Directory.createDirectory
  , narCreateLink = \f t -> IO.liftIO $ createSymbolicLink f t
  , narGetPerms   = IO.liftIO . Directory.getPermissions
  , narSetPerms   = \f p -> IO.liftIO $ Directory.setPermissions f p
  , narPathType   = \f -> fmap pathTypeFromPosix $ IO.liftIO (getSymbolicLinkStatus f)
  , narFileSize   = \n -> fmap (fromIntegral . fileSize) $ IO.liftIO (getSymbolicLinkStatus n)
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
  -> m (Maybe BS.ByteString)
  -> m ()
streamStringOutIO f getChunk =
  Lifted.bracket
    (IO.liftIO (IO.openFile f IO.WriteMode)) (IO.liftIO . IO.hClose) go
  `Lifted.catch`
    cleanupException
  where
    go :: IO.Handle -> m ()
    go handle = do
      chunk <- getChunk
      case chunk of
        Nothing -> return ()
        Just c  -> do
          IO.liftIO $ BS.hPut handle c
          go handle
    cleanupException (e :: Lifted.SomeException) = do
      IO.liftIO $ Directory.removeFile f
      MonadFail.fail $
        "Failed to stream string to " ++ f ++ ": " ++ show e
