module System.Nix.Store.Remote.Util where

import           Control.Monad.Reader

import           Data.Maybe
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.HashSet              as HashSet

import           Network.Socket.ByteString (recv, sendAll)

import           System.Nix.Store.Remote.Types
import           System.Nix.Hash
import           System.Nix.Util


genericIncremental :: (MonadIO m) => m (Maybe B.ByteString) -> Get a -> m a
genericIncremental getsome parser = go decoder
  where decoder = runGetIncremental parser
        go (Done _leftover _consumed x) = do
          return x
        go (Partial k) = do
          chunk <- getsome
          go (k chunk)
        go (Fail _leftover _consumed msg) = do
          error msg

getSocketIncremental :: Get a -> MonadStore s a
getSocketIncremental = genericIncremental sockGet

sockPut :: Put -> MonadStore s ()
sockPut p = do
  soc <- ask
  liftIO $ sendAll soc $ LBS.toStrict $ runPut p

sockGet :: MonadStore s (Maybe BSC.ByteString)
sockGet = do
  soc <- ask
  liftIO $ Just <$> recv soc 8

sockGetInt :: Integral a => MonadStore s a
sockGetInt = getSocketIncremental getInt

sockGetBool :: MonadStore s Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore s LBS.ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: MonadStore s [LBS.ByteString]
sockGetStrings = getSocketIncremental getByteStrings

lBSToText :: LBS.ByteString -> Text
lBSToText = T.pack . BSC.unpack . LBS.toStrict

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . BSC.pack . T.unpack

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)
