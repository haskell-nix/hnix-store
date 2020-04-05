module System.Nix.Store.Remote.Logger (
    Logger(..)
  , Field(..)
  , processOutput)
  where

import           Control.Exception

import           Control.Monad.Reader      (ask, liftIO)
import           Control.Monad.State       (modify)
import           Data.Binary.Get

import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.DList                (snoc)

import           Network.Socket.ByteString (recv)

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

processOutput :: MonadStore ()
processOutput = go decoder
  where decoder = runGetIncremental controlParser
        go :: Decoder Logger -> MonadStore ()
        go (Done _leftover _consumed ctrl) = do
          case ctrl of
            Error i e -> liftIO (throwIO (ProtocolError i (LBS.unpack e)))
            Last -> modify (`snoc` Last)
            -- we should probably handle Read here as well
            x -> do
              modify (`snoc` x)
              go decoder
        go (Partial k) = do
          soc <- ask
          chunk <- liftIO (Just <$> recv soc 8)
          go (k chunk)
        go (Fail _leftover _consumed msg) =
          liftIO (throwIO (ParseError msg))

getFields :: Get [Field]
getFields = do
  cnt <- getInt
  sequence $ replicate cnt getField

getField :: Get Field
getField = do
  typ <- getInt
  case (typ :: Int) of
    0 -> LogInt <$> getInt
    1 -> LogStr <$> getByteStringLen
    x -> fail $ "Unknown log type: " ++ show x
