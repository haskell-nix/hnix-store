{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote.Util where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Either
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as BSL

import           Network.Socket.ByteString (recv, sendAll)

import           Nix.Derivation

import           System.Nix.Build
import           System.Nix.StorePath
import           System.Nix.Store.Remote.Binary
import           System.Nix.Store.Remote.Types

import qualified Data.HashSet
import qualified Data.Map

genericIncremental :: (MonadIO m) => m (Maybe ByteString) -> Get a -> m a
genericIncremental getsome parser = go decoder
  where
    decoder = runGetIncremental parser
    go (Done _leftover _consumed x) = do
      return x
    go (Partial k) = do
      chunk <- getsome
      go (k chunk)
    go (Fail _leftover _consumed msg) = do
      error msg

getSocketIncremental :: (MonadIO m) => Get a -> MonadStoreT m a
getSocketIncremental = genericIncremental sockGet8
  where
    sockGet8 :: (MonadIO m) => MonadStoreT m (Maybe BSC.ByteString)
    sockGet8 = do
      soc <- storeSocket <$> NixStore ask
      liftIO $ Just <$> recv soc 8

sockPut :: (MonadIO m) => Put -> MonadStoreT m ()
sockPut p = do
  soc <- storeSocket <$> NixStore ask
  liftIO $ sendAll soc $ BSL.toStrict $ runPut p

sockGet :: (MonadIO m) => Get a -> MonadStoreT m a
sockGet = getSocketIncremental

sockGetInt :: (MonadIO m) => Integral a => MonadStoreT m a
sockGetInt = getSocketIncremental getInt

sockGetBool :: (MonadIO m) => MonadStoreT m Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: (MonadIO m) => MonadStoreT m ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: (MonadIO m) => MonadStoreT m [ByteString]
sockGetStrings = getSocketIncremental getByteStrings

sockGetPath :: (MonadIO m) => MonadStoreT m StorePath
sockGetPath = do
  sd <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  case pth of
    Left e -> throwError e
    Right x -> return x

sockGetPathMay :: (MonadIO m) => MonadStoreT m (Maybe StorePath)
sockGetPathMay = do
  sd <- getStoreDir
  pth <- getSocketIncremental (getPath sd)
  return $ case pth of
    Left _e -> Nothing
    Right x -> Just x

sockGetPaths :: (MonadIO m) => MonadStoreT m StorePathSet
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
putTexts = putByteStrings . (map textToBSL)

getPath :: FilePath -> Get (Either String StorePath)
getPath sd = parsePath sd <$> getByteStringLen

getPaths :: FilePath -> Get StorePathSet
getPaths sd = Data.HashSet.fromList . rights . map (parsePath sd) <$> getByteStrings

putPath :: StorePath -> Put
putPath  = putByteStringLen . BSL.fromStrict . storePathToRawFilePath

putPaths :: StorePathSet -> Put
putPaths = putByteStrings . Data.HashSet.toList . Data.HashSet.map (BSL.fromStrict . storePathToRawFilePath)

putBool :: Bool -> Put
putBool True  = putInt (1 :: Int)
putBool False = putInt (0 :: Int)

getBool :: Get Bool
getBool = (==1) <$> (getInt :: Get Int)

putEnum :: (Enum a) => a -> Put
putEnum = putInt . fromEnum

getEnum :: (Enum a) => Get a
getEnum = toEnum <$> getInt

putTime :: UTCTime -> Put
putTime = (putInt :: Int -> Put) . round . utcTimeToPOSIXSeconds

getTime :: Get UTCTime
getTime = posixSecondsToUTCTime <$> getEnum

getBuildResult :: Get BuildResult
getBuildResult = BuildResult
  <$> getEnum
  <*> (Just . bsToText <$> getByteStringLen)
  <*> getInt
  <*> getBool
  <*> getTime
  <*> getTime

putDerivation :: Derivation StorePath Text -> Put
putDerivation Derivation{..} = do
  flip putMany (Data.Map.toList outputs)
    $ \(outputName, DerivationOutput{..}) -> do
      putText outputName
      putPath path
      putText hashAlgo
      putText hash

  putMany putPath inputSrcs
  putText platform
  putText builder
  putMany putText args

  flip putMany (Data.Map.toList env)
    $ \(first, second) -> putText first >> putText second
