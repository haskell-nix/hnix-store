module System.Nix.Store.Remote.Socket where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, Result(..))
import Data.Serialize.Put (Put, runPut)
import Network.Socket.ByteString (recv, sendAll)
import System.Nix.Store.Remote.MonadStore (MonadRemoteStore(..), RemoteStoreError(..))
import System.Nix.Store.Remote.Serializer (NixSerializer, runP)
import System.Nix.Store.Remote.Types (ProtoStoreConfig)

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
  :: MonadRemoteStore m
  => m ByteString
sockGet8 = do
  soc <- getStoreSocket
  eresult <- liftIO $ Control.Exception.try $ recv soc 8
  case eresult of
    Left e ->
      throwError $ RemoteStoreError_IOException e

    Right result | Data.ByteString.length result == 0 ->
      throwError RemoteStoreError_Disconnected

    Right result | otherwise ->
      pure result

sockPut
  :: MonadRemoteStore m
  => Put
  -> m ()
sockPut p = do
  soc <- getStoreSocket
  liftIO $ sendAll soc $ runPut p

sockPutS
  :: ( MonadRemoteStore m
     , MonadError e m
     )
  => NixSerializer e a
  -> a
  -> m ()
sockPutS s a = do
  sock <- getStoreSocket
  let x = runP s a
  liftIO $ sendAll sock x

sockGetS
  :: ( MonadRemoteStore m
     , MonadError e m
     , Show a
     , Show e
     )
  => NixSerializer e a
  -> m a
sockGetS s = do
  res <- genericIncremental sockGet8
    $ runExceptT $ Data.Serializer.getS s

  case res of
    Right x -> pure x
    Left e -> throwError e
