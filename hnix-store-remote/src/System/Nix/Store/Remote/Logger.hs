module System.Nix.Store.Remote.Logger
  ( processOutput
  ) where

import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import Data.Serialize (Result(..))
import System.Nix.Store.Remote.Serialize.Prim (putByteString)
import System.Nix.Store.Remote.Serializer (LoggerSError, logger, runSerialT)
import System.Nix.Store.Remote.Socket (sockGet8, sockPut)
import System.Nix.Store.Remote.MonadStore (MonadStore, clearData)
import System.Nix.Store.Remote.Types (Logger(..), ProtoVersion, hasProtoVersion)

import qualified Control.Monad
import qualified Control.Monad.Reader
import qualified Control.Monad.State.Strict
import qualified Data.Serialize.Get
import qualified Data.Serializer

processOutput :: MonadStore [Logger]
processOutput = do
 protoVersion <- Control.Monad.Reader.asks hasProtoVersion
 sockGet8 >>= go . (decoder protoVersion)
 where
  decoder
    :: ProtoVersion
    -> ByteString
    -> Result (Either LoggerSError Logger)
  decoder protoVersion =
    Data.Serialize.Get.runGetPartial
      (runSerialT protoVersion $ Data.Serializer.getS logger)

  go :: Result (Either LoggerSError Logger) -> MonadStore [Logger]
  go (Done ectrl leftover) = do

    Control.Monad.unless (leftover == mempty) $
      -- TODO: throwError
      error $ "Leftovers detected: '" ++ show leftover ++ "'"

    protoVersion <- Control.Monad.Reader.asks hasProtoVersion
    case ectrl of
      -- TODO: tie this with throwError and better error type
      Left e -> error $ show e
      Right ctrl -> do
        case ctrl of
          e@(Logger_Error _) -> pure [e]
          Logger_Last -> pure [Logger_Last]
          Logger_Read _n -> do
            (mdata, _) <- Control.Monad.State.Strict.get
            case mdata of
              Nothing   -> throwError "No data to read provided"
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
