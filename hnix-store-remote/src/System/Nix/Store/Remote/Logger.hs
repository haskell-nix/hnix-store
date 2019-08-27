module System.Nix.Store.Remote.Logger (
    Logger(..)
  , Field(..)
  , streamLogs
  ) where

import           Control.Monad.Except      (throwError)
import           Control.Monad             (replicateM)
import           Control.Monad.Reader      (ask, liftIO)
import           Data.Binary.Get
import           Network.Socket.ByteString (recv)
import           Pipes                     (lift, yield)
import           System.Nix.Store.Remote.Types
import           System.Nix.Util

controlParser :: Get Logger
controlParser = do
  ctrl <- getInt
  case (ctrl :: Int) of
    0x6f6c6d67 -> Next          <$> getByteStringLen
    0x64617461 -> Read          <$> getInt
    0x64617416 -> Write         <$> getByteStringLen
    0x616c7473 -> pure Last
    0x63787470 -> flip Error    <$> getByteStringLen <*> getInt
    0x53545254 -> StartActivity <$> getInt <*> getInt <*> getInt <*> getByteStringLen <*> getFields <*> getInt
    0x53544f50 -> StopActivity  <$> getInt
    0x52534c54 -> Result        <$> getInt <*> getInt <*> getFields
    x          -> fail           $ "Invalid control message received:" ++ show x

logger :: Logger -> MonadStore ()
logger = lift . yield

streamLogs :: MonadStore ()
streamLogs = go decoder
  where
    go :: Decoder Logger -> MonadStore ()
    go (Done _leftover _consumed ctrl) = do
      case ctrl of
        e@(Error status err) -> do
          logger e
          throwError (LogError status err)
        Last ->
          logger Last
        -- we should probably handle Read here as well
        x -> do
          logger x
          go decoder
    go (Partial cont) = do
      soc <- ask
      chunk <- liftIO (recv soc 8)
      go (cont (Just chunk))
    go (Fail _leftover _consumed msg) =
      throwError (ParseError msg)

    decoder :: Decoder Logger
    decoder = runGetIncremental controlParser

getFields :: Get [Field]
getFields = do
  count <- getInt
  replicateM count getField

getField :: Get Field
getField = do
  typ <- getInt
  case (typ :: Int) of
    0 -> LogInt <$> getInt
    1 -> LogStr <$> getByteStringLen
    x -> fail $ "Unknown log type: " ++ show x
