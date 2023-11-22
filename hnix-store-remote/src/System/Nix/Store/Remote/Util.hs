module System.Nix.Store.Remote.Util where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Either (rights)

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

import Network.Socket.ByteString (recv, sendAll)

import System.Nix.Build
import System.Nix.Derivation
import System.Nix.StorePath (StoreDir, StorePath, InvalidPathError, parsePath, storePathToRawFilePath)
import System.Nix.Store.Remote.Binary
import System.Nix.Store.Remote.Types

import qualified Data.HashSet
import qualified Data.Map

genericIncremental :: (MonadIO m) => m (Maybe ByteString) -> Get a -> m a
genericIncremental getsome parser = go decoder
 where
  decoder = runGetIncremental parser
  go (Done _leftover _consumed x  ) = pure x
  go (Partial k                   ) = do
    chunk <- getsome
    go (k chunk)
  go (Fail _leftover _consumed msg) = error msg

getSocketIncremental :: Get a -> MonadStore a
getSocketIncremental = genericIncremental sockGet8
 where
  sockGet8 :: MonadStore (Maybe BSC.ByteString)
  sockGet8 = do
    soc <- asks storeSocket
    liftIO $ Just <$> recv soc 8

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- asks storeSocket
  liftIO $ sendAll soc $ BSL.toStrict $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = getSocketIncremental

sockGetInt :: Integral a => MonadStore a
sockGetInt = getSocketIncremental getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: MonadStore [ByteString]
sockGetStrings = getSocketIncremental getByteStrings

sockGetPath :: MonadStore StorePath
sockGetPath = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  either
    (throwError . show)
    pure
    pth

sockGetPathMay :: MonadStore (Maybe StorePath)
sockGetPathMay = do
  sd  <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  pure $
    either
      (const Nothing)
      Just
      pth

sockGetPaths :: MonadStore (HashSet StorePath)
sockGetPaths = do
  sd <- getStoreDir
  getSocketIncremental (getPaths sd)

bsToText :: ByteString -> Text
bsToText = T.decodeUtf8

textToBS :: Text -> ByteString
textToBS = T.encodeUtf8

bslToText :: BSL.ByteString -> Text
bslToText = TL.toStrict . TL.decodeUtf8

textToBSL :: Text -> BSL.ByteString
textToBSL = TL.encodeUtf8 . TL.fromStrict

putText :: Text -> Put
putText = putByteStringLen . textToBSL

putTexts :: [Text] -> Put
putTexts = putByteStrings . fmap textToBSL

getPath :: StoreDir -> Get (Either InvalidPathError StorePath)
getPath sd = parsePath sd <$> getByteStringLen

getPaths :: StoreDir -> Get (HashSet StorePath)
getPaths sd =
  Data.HashSet.fromList . rights . fmap (parsePath sd) <$> getByteStrings

putPath :: StoreDir -> StorePath -> Put
putPath storeDir = putByteStringLen . BSL.fromStrict . storePathToRawFilePath storeDir

putPaths :: StoreDir -> HashSet StorePath -> Put
putPaths storeDir = putByteStrings . Data.HashSet.toList . Data.HashSet.map
  (BSL.fromStrict . storePathToRawFilePath storeDir)

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)

getBool :: Get Bool
getBool = (== 1) <$> (getInt :: Get Int)

putEnum :: (Enum a) => a -> Put
putEnum = putInt . fromEnum

getEnum :: (Enum a) => Get a
getEnum = toEnum <$> getInt

putTime :: UTCTime -> Put
putTime = (putInt :: Int -> Put) . round . utcTimeToPOSIXSeconds

getTime :: Get UTCTime
getTime = posixSecondsToUTCTime <$> getEnum

getBuildResult :: Get BuildResult
getBuildResult =
  BuildResult
    <$> getEnum
    <*> (Just . bsToText <$> getByteStringLen)
    <*> getInt
    <*> getBool
    <*> getTime
    <*> getTime

putDerivation :: StoreDir -> Derivation StorePath Text -> Put
putDerivation storeDir Derivation{..} = do
  flip putMany (Data.Map.toList outputs)
    $ \(outputName, DerivationOutput{..}) -> do
        putText outputName
        putPath storeDir path
        putText hashAlgo
        putText hash

  putMany (putPath storeDir) inputSrcs
  putText platform
  putText builder
  putMany putText args

  flip putMany (Data.Map.toList env)
    $ \(a1, a2) -> putText a1 *> putText a2
