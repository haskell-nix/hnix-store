{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote.Util where

import           Control.Monad.Reader

import           Prelude                   hiding (FilePath)

import           Data.Maybe
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.HashMap.Strict       as HashMap
import qualified Data.HashSet              as HashSet
import qualified Data.Vector

import qualified Filesystem.Path.CurrentOS
import           Filesystem.Path.CurrentOS (FilePath)

import           Network.Socket.ByteString (recv, sendAll)

import           Nix.Derivation

import           System.Nix.Store.Remote.Types
import           System.Nix.Build
import qualified System.Nix.Hash           as Hash
import           System.Nix.Path
import           System.Nix.Internal.Path
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

getSocketIncremental :: Get a -> MonadStore a
getSocketIncremental = genericIncremental sockGet8
  where
    sockGet8 :: MonadStore (Maybe BSC.ByteString)
    sockGet8 = do
      soc <- storeSocket <$> ask
      liftIO $ Just <$> recv soc 8

sockPut :: Put -> MonadStore ()
sockPut p = do
  soc <- storeSocket <$> ask
  liftIO $ sendAll soc $ LBS.toStrict $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = getSocketIncremental

sockGetPath :: MonadStore (Maybe Path)
sockGetPath = getSocketIncremental getPath

sockGetPaths :: MonadStore PathSet
sockGetPaths = getSocketIncremental getPaths

sockGetInt :: Integral a => MonadStore a
sockGetInt = getSocketIncremental getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore LBS.ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: MonadStore [LBS.ByteString]
sockGetStrings = getSocketIncremental getByteStrings

lBSToText :: LBS.ByteString -> Text
lBSToText = T.pack . BSC.unpack . LBS.toStrict

textToLBS :: Text -> LBS.ByteString
textToLBS = LBS.fromStrict . BSC.pack . T.unpack

putText :: Text -> Put
putText = putByteStringLen . textToLBS

putTexts :: [Text] -> Put
putTexts = putByteStrings . (map textToLBS)

getPath :: Get (Maybe Path)
getPath = parsePath <$> getByteStringLen

getPaths :: Get PathSet
getPaths = HashSet.fromList . catMaybes . map parsePath <$> getByteStrings

putPath :: StoreDir -> Path -> Put
putPath sd  = putText . storedToText . makeStored sd

putPaths :: StoreDir -> PathSet -> Put
putPaths sd = putTexts . HashSet.toList .  HashSet.map (storedToText . makeStored sd)

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
  <*> (Just . lBSToText <$> getByteStringLen)
  <*> getInt
  <*> getBool
  <*> getTime
  <*> getTime

putHashAlgo :: Hash.HashAlgorithm -> Put
putHashAlgo Hash.MD5 = putText "md5"
putHashAlgo Hash.SHA1 = putText "sha1"
putHashAlgo Hash.SHA256 = putText "sha256"
putHashAlgo (Hash.Truncated _ algo) = putHashAlgo algo

putDerivation :: Derivation -> Put
putDerivation Derivation{..} = do
  putInt $ M.size outputs
  forM_ (M.toList outputs) $ \(outputName, DerivationOutput{..}) -> do
    putText outputName
    putFP path
    putText hashAlgo
    putText hash
    --putText $ printAsBase32 @PathHashAlgo digest

  putFPs $ S.toList inputSrcs
  putText platform
  putText builder
  putTexts $ Data.Vector.toList args

  putInt $ M.size env
  forM_ (M.toList env) $ \(first, second) -> putText first >> putText second

putFP :: FilePath -> Put
putFP p = putText (printFP p)

printFP :: FilePath -> Text
printFP p = case Filesystem.Path.CurrentOS.toText p of
  Left t -> t
  Right t -> t

putFPs :: [FilePath] -> Put
putFPs = putTexts . (map printFP)
