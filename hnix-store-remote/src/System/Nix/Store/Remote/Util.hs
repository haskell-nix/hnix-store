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
import qualified Data.ByteString.Lazy      as BSL
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
import           System.Nix.StorePath
import           System.Nix.Internal.Hash (Digest(..))
import           System.Nix.Hash
import           System.Nix.Util

import           System.FilePath.Posix     (takeBaseName, takeDirectory)


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
  liftIO $ sendAll soc $ BSL.toStrict $ runPut p

sockGet :: Get a -> MonadStore a
sockGet = getSocketIncremental

sockGetInt :: Integral a => MonadStore a
sockGetInt = getSocketIncremental getInt

sockGetBool :: MonadStore Bool
sockGetBool = (== (1 :: Int)) <$> sockGetInt

sockGetStr :: MonadStore BSL.ByteString
sockGetStr = getSocketIncremental getByteStringLen

sockGetStrings :: MonadStore [BSL.ByteString]
sockGetStrings = getSocketIncremental getByteStrings

sockGetPath :: (KnownStoreDir a) => MonadStore (Maybe (StorePath a))
sockGetPath = getSocketIncremental getPath

sockGetPaths :: (KnownStoreDir a) => MonadStore (StorePathSet a)
sockGetPaths = getSocketIncremental getPaths


lBSToText :: BSL.ByteString -> Text
lBSToText = T.pack . BSC.unpack . BSL.toStrict

textToBSL :: Text -> BSL.ByteString
textToBSL = BSL.fromStrict . BSC.pack . T.unpack

putText :: Text -> Put
putText = putByteStringLen . textToBSL

putTexts :: [Text] -> Put
putTexts = putByteStrings . (map textToBSL)

parsePath :: (KnownStoreDir storeDir) => BSL.ByteString -> Maybe (StorePath storeDir)
parsePath p = case name of
    Nothing -> Nothing
    Just n -> Just $ StorePath digest n
  where
  base = T.pack . takeBaseName . BSC.unpack . BSL.toStrict $ p 
  parts = T.breakOn "-" base
  digest = Digest . BSC.pack . T.unpack . fst $ parts
  name = makeStorePathName . T.drop 1 . snd $ parts

getPath :: (KnownStoreDir storeDir) => Get (Maybe (StorePath storeDir))
getPath = parsePath <$> getByteStringLen

getPaths :: (KnownStoreDir storeDir) => Get (StorePathSet storeDir)
getPaths = HashSet.fromList . catMaybes . map parsePath <$> getByteStrings

putPath :: (KnownStoreDir storeDir) => StorePath storeDir -> Put
putPath  = putByteStringLen . BSL.fromStrict . storePathToRawFilePath

putPaths :: (KnownStoreDir storeDir) => StorePathSet storeDir -> Put
putPaths = putByteStrings . HashSet.toList .  HashSet.map (BSL.fromStrict . storePathToRawFilePath)

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

{-
putDerivation :: (NamedAlgo a) => Derivation -> Put
putDerivation Derivation{..} = do
  putInt $ M.size outputs
  forM_ (M.toList outputs) $ \(outputName, DerivationOutput{..}) -> do
    putText outputName
    putFP path
    putText algoName
    putText hash
    --putText $ printAsBase32 @PathHashAlgo digest

  putFPs $ S.toList inputSrcs
  putText platform
  putText builder
  putTexts $ Data.Vector.toList args

  putInt $ M.size env
  forM_ (M.toList env) $ \(first, second) -> putText first >> putText second
-}

putFP :: FilePath -> Put
putFP p = putText (printFP p)

printFP :: FilePath -> Text
printFP p = case Filesystem.Path.CurrentOS.toText p of
  Left t -> t
  Right t -> t

putFPs :: [FilePath] -> Put
putFPs = putTexts . (map printFP)
