module System.Nix.Store.Remote.Util where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)

import Data.Serialize.Get (Get, Result(..))
import Data.Serialize.Put
import Network.Socket.ByteString (recv, sendAll)

import System.Nix.StorePath (StorePath)
import System.Nix.Store.Remote.Types

import qualified System.Nix.Store.Remote.Serialize.Prim as P
import qualified Data.Serialize.Get

genericIncremental
  :: MonadIO m
  => m ByteString
  -> Get a
  -> m a
genericIncremental getsome parser = do
  getsome >>= go . decoder
 where
  decoder = Data.Serialize.Get.runGetPartial parser
  go (Done x _leftover) = pure x
  go (Partial k) = do
    chunk <- getsome
    go (k chunk)
  go (Fail msg _leftover) = error msg

getSocketIncremental :: Get a -> MonadStore a
getSocketIncremental = genericIncremental sockGet8
 where
  sockGet8 :: MonadStore ByteString
  sockGet8 = do
    soc <- asks storeSocket
    liftIO $ recv soc 8

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- asks storeSocket
  liftIO $ sendAll soc $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = getSocketIncremental

sockGetInt :: Integral a => MonadStore a
sockGetInt = fromIntegral <$> getSocketIncremental P.getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore ByteString
sockGetStr = getSocketIncremental P.getByteString

sockGetStrings :: MonadStore [ByteString]
sockGetStrings = getSocketIncremental P.getByteStrings

sockGetPath :: MonadStore StorePath
sockGetPath = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (P.getPath sd)
  either
    (throwError . show)
    pure
    pth

sockGetPathMay :: MonadStore (Maybe StorePath)
sockGetPathMay = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (P.getPath sd)
  pure $
    either
      (const Nothing)
      Just
      pth

sockGetPaths :: MonadStore (HashSet StorePath)
sockGetPaths = do
  sd <- getStoreDir
  getSocketIncremental (P.getPathsOrFail sd)
