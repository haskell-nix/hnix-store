module System.Nix.Store.Remote.Socket where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Serialize.Get (Get, Result(..))
import Data.Serialize.Put (Put, runPut)
import Network.Socket.ByteString (recv, sendAll)
import System.Nix.StorePath (HasStoreDir, StorePath)
import System.Nix.Store.Remote.MonadStore (RemoteStoreT, RemoteStoreError(..), getStoreDir)
import System.Nix.Store.Remote.Serializer (NixSerializer, runP, runSerialT)
import System.Nix.Store.Remote.Serialize.Prim (getInt, getByteString, getByteStrings, getPath, getPathsOrFail)
import System.Nix.Store.Remote.Types (HasStoreSocket(..))

import qualified Data.ByteString
import qualified Data.Serializer
import qualified Data.Serialize.Get

genericIncremental
  :: MonadIO m
  => m ByteString
  -> Get a
  -> m a
genericIncremental getsome parser = do
  getsome >>= go . decoder
 where
  decoder = Data.Serialize.Get.runGetPartial parser
  go (Done x _leftover) = pure x
  go (Partial k) = do
    chunk <- getsome
    go (k chunk)
  go (Fail msg _leftover) = error msg

sockGet8
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => RemoteStoreT r m ByteString
sockGet8 = do
  soc <- asks hasStoreSocket
  liftIO $ recv soc 8

sockPut
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => Put
  -> RemoteStoreT r m ()
sockPut p = do
  soc <- asks hasStoreSocket
  liftIO $ sendAll soc $ runPut p

sockPutS
  :: ( MonadReader r m
     , MonadError e m
     , MonadIO m
     , HasStoreSocket r
     )
  => NixSerializer r e a
  -> a
  -> m ()
sockPutS s a = do
  r <- ask
  case runP s r a of
    Right x -> liftIO $ sendAll (hasStoreSocket r) x
    Left e -> throwError e

sockGetS
  :: forall r e m a
   . ( HasStoreSocket r
     , MonadError RemoteStoreError m
     , MonadError e m
     , MonadReader r m
     , MonadIO m
     )
  => NixSerializer r e a
  -> m a
sockGetS s = do
  r <- ask
  res <- genericIncremental sockGet8'
    $ runSerialT r $ Data.Serializer.getS s

  case res of
    Right x -> pure x
    Left e -> throwError e
 where
  sockGet8' :: MonadError RemoteStoreError m => m ByteString
  sockGet8' = do
    soc <- asks hasStoreSocket
    result <- liftIO $ recv soc 8
    if Data.ByteString.length result == 0
      then throwError RemoteStoreError_Disconnected
      else pure result

-- * Obsolete

getSocketIncremental
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => Get a
  -> RemoteStoreT r m a
getSocketIncremental = genericIncremental sockGet8

sockGet
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => Get a
  -> RemoteStoreT r m a
sockGet = getSocketIncremental

sockGetInt
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     , Integral a
     )
  => RemoteStoreT r m a
sockGetInt = getSocketIncremental getInt

sockGetBool
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => RemoteStoreT r m Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => RemoteStoreT r m ByteString
sockGetStr = getSocketIncremental getByteString

sockGetStrings
  :: ( Monad m
     , MonadIO m
     , HasStoreSocket r
     )
  => RemoteStoreT r m [ByteString]
sockGetStrings = getSocketIncremental getByteStrings

sockGetPath
  :: ( Monad m
     , MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     )
  => RemoteStoreT r m StorePath
sockGetPath = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  either
    (throwError . RemoteStoreError_Fixme . show)
    pure
    pth

sockGetPathMay
  :: ( Monad m
     , MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     )
  => RemoteStoreT r m (Maybe StorePath)
sockGetPathMay = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  pure $
    either
      (const Nothing)
      Just
      pth

sockGetPaths
  :: ( Monad m
     , MonadIO m
     , HasStoreDir r
     , HasStoreSocket r
     )
  => RemoteStoreT r m (HashSet StorePath)
sockGetPaths = do
  sd <- getStoreDir
  getSocketIncremental (getPathsOrFail sd)
