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
import System.Nix.Store.Remote.MonadStore (MonadRemoteStore0, RemoteStoreError(..), getStoreDir, getStoreSocket)
import System.Nix.Store.Remote.Serializer (NixSerializer, SError, runP, runSerialT)
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
  :: HasStoreSocket r
  => MonadRemoteStore0 r ByteString
sockGet8 = do
  soc <- getStoreSocket
  liftIO $ recv soc 8

sockPut
  :: HasStoreSocket r
  => Put
  -> MonadRemoteStore0 r ()
sockPut p = do
  soc <- getStoreSocket
  liftIO $ sendAll soc $ runPut p

sockPutS
  :: ( MonadReader r m
     , MonadError RemoteStoreError m
     , MonadIO m
     , HasStoreSocket r
     )
  => NixSerializer r SError a
  -> a
  -> m ()
sockPutS s a = do
  r <- ask
  case runP s r a of
    Right x -> liftIO $ sendAll (hasStoreSocket r) x
    Left e -> throwError $ RemoteStoreError_SerializerPut e

sockGetS
  :: forall r m a
   . ( HasStoreSocket r
     , MonadError RemoteStoreError m
     , MonadReader r m
     , MonadIO m
     )
  => NixSerializer r SError a
  -> m a
sockGetS s = do
  r <- ask
  res <- genericIncremental sockGet8'
    $ runSerialT r $ Data.Serializer.getS s

  case res of
    Right x -> pure x
    Left e -> throwError $ RemoteStoreError_SerializerGet e
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
  :: HasStoreSocket r
  => Get a
  -> MonadRemoteStore0 r a
getSocketIncremental = genericIncremental sockGet8

sockGet
  :: HasStoreSocket r
  => Get a
  -> MonadRemoteStore0 r a
sockGet = getSocketIncremental

sockGetInt
  :: ( HasStoreSocket r
     , Integral a
     )
  => MonadRemoteStore0 r a
sockGetInt = getSocketIncremental getInt

sockGetBool
  :: HasStoreSocket r
  => MonadRemoteStore0 r Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr
  :: HasStoreSocket r
  => MonadRemoteStore0 r ByteString
sockGetStr = getSocketIncremental getByteString

sockGetStrings
  :: HasStoreSocket r
  => MonadRemoteStore0 r [ByteString]
sockGetStrings = getSocketIncremental getByteStrings

sockGetPath
  :: ( HasStoreDir r
     , HasStoreSocket r
     )
  => MonadRemoteStore0 r StorePath
sockGetPath = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  either
    (throwError . RemoteStoreError_Fixme . show)
    pure
    pth

sockGetPathMay
  :: ( HasStoreDir r
     , HasStoreSocket r
     )
  => MonadRemoteStore0 r (Maybe StorePath)
sockGetPathMay = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  pure $
    either
      (const Nothing)
      Just
      pth

sockGetPaths
  :: ( HasStoreDir r
     , HasStoreSocket r
     )
  => MonadRemoteStore0 r (HashSet StorePath)
sockGetPaths = do
  sd <- getStoreDir
  getSocketIncremental (getPathsOrFail sd)
