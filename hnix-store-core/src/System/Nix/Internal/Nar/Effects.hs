-- |

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Nix.Internal.Nar.Effects
  ( NarEffects(..)
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
import           System.Posix.Files          (createSymbolicLink, fileSize,
                                              getFileStatus, isDirectory,
                                              readSymbolicLink)

data NarEffects (m :: * -> *) = NarEffects {
    narReadFile   :: FilePath -> m BSL.ByteString
  , narWriteFile  :: FilePath -> BSL.ByteString -> m ()
  , narStreamFile :: FilePath -> m (Maybe BS.ByteString) -> m ()
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
    narReadFile   = IO.liftIO . BSL.readFile
  , narWriteFile  = \a b -> IO.liftIO $ BSL.writeFile a b
  , narStreamFile = streamStringOutIO
  , narListDir    = IO.liftIO . Directory.listDirectory
  , narCreateDir  = IO.liftIO . Directory.createDirectory
  , narCreateLink = \f t -> IO.liftIO $ createSymbolicLink f t
  , narGetPerms   = IO.liftIO . Directory.getPermissions
  , narSetPerms   = \f p -> IO.liftIO $ Directory.setPermissions f p
  , narIsDir      = \d -> fmap isDirectory $ IO.liftIO (getFileStatus d)
  , narIsSymLink  = IO.liftIO . Directory.pathIsSymbolicLink
  , narFileSize   = \n -> fmap (fromIntegral . fileSize) $ IO.liftIO (getFileStatus n)
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
streamStringOutIO f getChunk = do
  h <- IO.liftIO $ do
    IO.openFile f IO.WriteMode
  let
    go = do
      chunk <- getChunk
      case chunk of
        Nothing -> IO.liftIO $ IO.hClose h
        Just c  -> do
          IO.liftIO $ BS.hPut h c
          go
    cleanupException (e :: Lifted.SomeException) = do
      IO.liftIO $ Directory.removeFile f
      MonadFail.fail $
        "Failed to stream string to " ++ f ++ ": " ++ show e

  go `Lifted.catch` cleanupException
