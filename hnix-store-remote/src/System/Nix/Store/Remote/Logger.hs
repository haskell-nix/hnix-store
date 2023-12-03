module System.Nix.Store.Remote.Logger
  ( processOutput
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Serialize (Result(..))
import System.Nix.Store.Remote.Serializer (LoggerSError, logger, runSerialT)
import System.Nix.Store.Remote.Socket (sockGet8)
import System.Nix.Store.Remote.MonadStore (MonadRemoteStore, RemoteStoreError(..), appendLog, getDataSource, getStoreSocket, getProtoVersion, setError)
import System.Nix.Store.Remote.Types.Logger (Logger(..))
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion)

import qualified Control.Monad
import qualified Data.Serialize.Get
import qualified Data.Serializer
import qualified Network.Socket.ByteString

processOutput
  :: MonadRemoteStore m
  => m ()
processOutput = do
 protoVersion <- getProtoVersion
 sockGet8 >>= go . (decoder protoVersion)
 where
  decoder
    :: ProtoVersion
    -> ByteString
    -> Result (Either LoggerSError Logger)
  decoder protoVersion =
    Data.Serialize.Get.runGetPartial
      (runSerialT protoVersion $ Data.Serializer.getS logger)

  go
    :: MonadRemoteStore m
    => Result (Either LoggerSError Logger)
    -> m ()
  go (Done ectrl leftover) = do
    let loop = do
          protoVersion <- getProtoVersion
          sockGet8 >>= go . (decoder protoVersion)

    Control.Monad.unless (leftover == mempty) $
      throwError
      $ RemoteStoreError_LoggerLeftovers
          (show ectrl)
          leftover

    case ectrl of
      Left e -> throwError $ RemoteStoreError_SerializerLogger e
      Right ctrl -> do
        case ctrl of
          -- These two terminate the logger loop
          e@(Logger_Error _) -> setError >> appendLog e
          Logger_Last -> appendLog Logger_Last

          -- Read data from source
          Logger_Read size -> do
            mSource <- getDataSource
            case mSource of
              Nothing   ->
                throwError RemoteStoreError_NoDataSourceProvided
              Just source -> do
                mChunk <- liftIO $ source size
                case mChunk of
                  Nothing -> throwError RemoteStoreError_DataSourceExhausted
                  Just chunk -> do
                    sock <- getStoreSocket
                    liftIO $ Network.Socket.ByteString.sendAll sock chunk

            loop

          -- Write data to sink
          -- used with tunnel sink in ExportPath operation
          Logger_Write _out -> do
            -- TODO: handle me
            loop

          -- Following we just append and loop
          -- but listed here explicitely for posterity
          x@(Logger_Next _) -> appendLog x >> loop
          x@(Logger_StartActivity {}) -> appendLog x >> loop
          x@(Logger_StopActivity {}) -> appendLog x >> loop
          x@(Logger_Result {}) -> appendLog x >> loop

  go (Partial k) = do
    chunk <- sockGet8
    go (k chunk)

  go (Fail msg leftover) =
    throwError
    $ RemoteStoreError_LoggerParserFail
        msg
        leftover
