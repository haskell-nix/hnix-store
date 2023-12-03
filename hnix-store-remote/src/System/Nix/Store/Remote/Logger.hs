module System.Nix.Store.Remote.Logger
  ( processOutput
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.Serialize (Result(..))
import System.Nix.StorePath (HasStoreDir(..))
import System.Nix.Store.Remote.Serialize.Prim (putByteString)
import System.Nix.Store.Remote.Serializer (LoggerSError, logger, runSerialT)
import System.Nix.Store.Remote.Socket (sockGet8, sockPut)
import System.Nix.Store.Remote.MonadStore (RemoteStoreT, RemoteStoreError(..), clearData, getData, getProtoVersion)
import System.Nix.Store.Remote.Types.Logger (Logger(..))
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion)
import System.Nix.Store.Remote.Types.StoreConfig (HasStoreSocket(..))

import qualified Control.Monad
import qualified Data.Serialize.Get
import qualified Data.Serializer

processOutput
  :: ( Monad m
     , MonadIO m
     , HasProtoVersion r
     , HasStoreDir r
     , HasStoreSocket r
     )
  => RemoteStoreT r m [Logger]
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
    :: ( Monad m
       , MonadIO m
       , HasProtoVersion r
       , HasStoreDir r
       , HasStoreSocket r
       )
    => Result (Either LoggerSError Logger)
    -> RemoteStoreT r m [Logger]
  go (Done ectrl leftover) = do

    Control.Monad.unless (leftover == mempty) $
      -- TODO: throwError
      error $ "Leftovers detected: '" ++ show leftover ++ "'"

    protoVersion <- getProtoVersion
    case ectrl of
      -- TODO: tie this with throwError and better error type
      Left e -> error $ show e
      Right ctrl -> do
        case ctrl of
          e@(Logger_Error _) -> pure [e]
          Logger_Last -> pure [Logger_Last]
          Logger_Read _n -> do
            mdata <- getData
            case mdata of
              Nothing   -> throwError RemoteStoreError_NoDataProvided
              Just part -> do
                -- XXX: we should check/assert part size against n of (Read n)
                sockPut $ putByteString part
                clearData

            sockGet8 >>= go . (decoder protoVersion)

          -- we should probably handle Read here as well
          x -> do
            next <- sockGet8 >>= go . (decoder protoVersion)
            pure $ x : next
  go (Partial k) = do
    chunk <- sockGet8
    go (k chunk)

  go (Fail msg _leftover) = error msg
