module System.Nix.Store.Remote.Socket where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, Result(..))
import Data.Serialize.Put (Put, runPut)
import Network.Socket.ByteString (recv, sendAll)
import System.Nix.Store.Remote.MonadStore (MonadRemoteStoreR, RemoteStoreError(..))
import System.Nix.Store.Remote.Serializer (NixSerializer, runP, runSerialT)
import System.Nix.Store.Remote.Types (HasStoreSocket(..))

import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.Serializer
import qualified Data.Serialize.Get

genericIncremental
  :: ( MonadIO m
     , MonadError RemoteStoreError m
     , Show a
     )
  => m ByteString
  -> Get a
  -> m a
genericIncremental getsome parser = do
  getsome >>= go . decoder
 where
  decoder = Data.Serialize.Get.runGetPartial parser
  go (Done x leftover) | leftover /= mempty =
    throwError
    $ RemoteStoreError_GenericIncrementalLeftovers
        (show x)
        leftover

  go (Done x _leftover) = pure x

  go (Partial k) = do
    chunk <- getsome
    go (k chunk)

  go (Fail msg leftover) =
    throwError
    $ RemoteStoreError_GenericIncrementalFail
        msg
        leftover

sockGet8
  :: ( MonadIO m
     , MonadError RemoteStoreError m
     , MonadReader r m
     , HasStoreSocket r
     )
  => m ByteString
sockGet8 = do
  soc <- asks hasStoreSocket
  eresult <- liftIO $ Control.Exception.try $ recv soc 8
  case eresult of
    Left e ->
      throwError $ RemoteStoreError_IOException e

    Right result | Data.ByteString.length result == 0 ->
      throwError RemoteStoreError_Disconnected

    Right result | otherwise ->
      pure result

sockPut
  :: ( MonadRemoteStoreR r m
     , HasStoreSocket r
     )
  => Put
  -> m ()
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
     , Show a
     , Show e
     )
  => NixSerializer r e a
  -> m a
sockGetS s = do
  r <- ask
  res <- genericIncremental sockGet8
    $ runSerialT r $ Data.Serializer.getS s

  case res of
    Right x -> pure x
    Left e -> throwError e
