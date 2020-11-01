{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.Store.Remote.Logger (
    Logger(..)
  , Field(..)
  , processOutput)
  where

import           Control.Monad.Except
import           Control.Monad.Reader      (ask)
import           Control.Monad.State       (get)
import           Data.Binary.Get

import           Network.Socket.ByteString (recv)

import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Util

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

processOutput :: MonadIO m => MonadStoreT m [Logger]
processOutput = go decoder
  where decoder = runGetIncremental controlParser
        go :: MonadIO m => Decoder Logger -> MonadStoreT m [Logger]
        go (Done _leftover _consumed ctrl) = do
          case ctrl of
            e@(Error _ _) -> return [e]
            Last -> return [Last]
            Read _n -> do
              (mdata, _) <- NixStore get
              case mdata of
                Nothing -> throwError "No data to read provided"
                Just part -> do
                  -- XXX: we should check/assert part size against n of (Read n)
                  sockPut $ putByteStringLen part
                  clearData

              next <- go decoder
              return $ next

            -- we should probably handle Read here as well
            x -> do
              next <- go decoder
              return $ x:next
        go (Partial k) = do
          soc <- storeSocket <$> NixStore ask
          chunk <- liftIO (Just <$> recv soc 8)
          go (k chunk)

        go (Fail _leftover _consumed msg) = do
          error msg

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
