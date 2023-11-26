module System.Nix.Store.Remote.Logger
  ( Logger(..)
  , Field(..)
  , processOutput
  ) where

import Control.Monad.Except (throwError)
import Data.Serialize (Get, Result(..))
import System.Nix.Store.Remote.Serialize ()
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Socket
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Types

import qualified Control.Monad
import qualified Control.Monad.State.Strict
import qualified Data.Serialize
import qualified Data.Serialize.Get

controlParser :: Get Logger
controlParser = do
  ctrl <- Data.Serialize.get
  case (ctrl :: LoggerOpCode) of
    LoggerOpCode_Next ->
      Next <$> getByteString
    LoggerOpCode_Read ->
      Read <$> getInt
    LoggerOpCode_Write ->
      Write <$> getByteString
    LoggerOpCode_Last ->
      pure Last
    LoggerOpCode_Error ->
      flip Error <$> getByteString
                 <*> getInt
    LoggerOpCode_StartActivity ->
      StartActivity <$> (ActivityID <$> getInt)
                    <*> Data.Serialize.get
                    <*> getInt
                    <*> getByteString
                    <*> getFields
                    <*> (ActivityID <$> getInt)
    LoggerOpCode_StopActivity ->
      StopActivity  <$> (ActivityID <$> getInt)
    LoggerOpCode_Result ->
      Result <$> (ActivityID <$> getInt)
             <*> getInt
             <*> getFields

processOutput :: MonadStore [Logger]
processOutput = do
 sockGet8 >>= go . decoder
 where
  decoder = Data.Serialize.Get.runGetPartial controlParser
  go :: Result Logger -> MonadStore [Logger]
  go (Done ctrl _leftover) = do
    case ctrl of
      e@(Error _ _) -> pure [e]
      Last          -> pure [Last]
      Read _n       -> do
        (mdata, _) <- Control.Monad.State.Strict.get
        case mdata of
          Nothing   -> throwError "No data to read provided"
          Just part -> do
            -- XXX: we should check/assert part size against n of (Read n)
            sockPut $ putByteString part
            clearData

        sockGet8 >>= go . decoder

      -- we should probably handle Read here as well
      x -> do
        next <- sockGet8 >>= go . decoder
        pure $ x : next
  go (Partial k) = do
    chunk <- sockGet8
    go (k chunk)

  go (Fail msg _leftover) = error msg

getFields :: Get [Field]
getFields = do
  cnt <- getInt
  Control.Monad.replicateM cnt Data.Serialize.get
