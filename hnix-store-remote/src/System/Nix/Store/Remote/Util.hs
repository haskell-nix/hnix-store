module System.Nix.Store.Remote.Util where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (ask, liftIO)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS

import           Network.Socket.ByteString (recv, sendAll)

import           System.Nix.Store.Remote.Types
import           System.Nix.Util

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- ask
  liftIO $ sendAll soc $ LBS.toStrict $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = go . runGetIncremental
  where
    go :: Decoder a -> MonadStore a
    go (Done _leftover _consumed x) = return x
    go (Partial cont) = do
      sock <- ask
      chunk <- liftIO (recv sock 8)
      go (cont (Just chunk))
    go (Fail _leftover _consumed msg) =
      throwError (ParseError msg)

sockGetInt :: MonadStore Int
sockGetInt = sockGet getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore LBS.ByteString
sockGetStr = sockGet getByteStringLen

sockGetStrings :: MonadStore [LBS.ByteString]
sockGetStrings = sockGet getByteStrings

lBSToText :: LBS.ByteString -> Text
lBSToText = T.pack . BSC.unpack . LBS.toStrict

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . BSC.pack . T.unpack

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)
