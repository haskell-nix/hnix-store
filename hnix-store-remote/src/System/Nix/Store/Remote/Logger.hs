module System.Nix.Store.Remote.Logger
  ( Logger(..)
  , Field(..)
  , processOutput
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (get)
import Data.Serialize.Get (Get, Result(..))
import System.Nix.Store.Remote.Serialize ()
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Socket
import System.Nix.Store.Remote.MonadStore
import System.Nix.Store.Remote.Types

import qualified Control.Monad
import qualified Data.Serialize
import qualified Data.Serialize.Get

controlParser :: Get Logger
controlParser = do
  ctrl <- getInt
  case (ctrl :: Int) of
    0x6f6c6d67 -> Next          <$> getByteString
    0x64617461 -> Read          <$> getInt
    0x64617416 -> Write         <$> getByteString
    0x616c7473 -> pure Last
    0x63787470 -> flip Error    <$> getByteString
                                <*> getInt
    0x53545254 -> StartActivity <$> (ActivityID <$> getInt)
                                <*> Data.Serialize.get
                                <*> getInt
                                <*> getByteString
                                <*> getFields
                                <*> (ActivityID <$> getInt)
    0x53544f50 -> StopActivity  <$> (ActivityID <$> getInt)
    0x52534c54 -> Result        <$> (ActivityID <$> getInt)
                                <*> getInt
                                <*> getFields
    x          -> fail          $ "Invalid control message received:" <> show x

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
        (mdata, _) <- get
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
