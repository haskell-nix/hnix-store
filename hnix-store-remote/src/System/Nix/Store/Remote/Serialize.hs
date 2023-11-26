{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Description : Serialize instances for complex types
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Store.Remote.Serialize where

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get)
import Data.Serialize.Put (Putter)
import Data.Text (Text)
import Data.Word (Word8)

import qualified Data.Bool
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Vector

import System.Nix.Derivation (Derivation(..), DerivationOutput(..))
import System.Nix.Build (BuildMode(..), BuildStatus(..), BuildResult(..))
import System.Nix.StorePath (StoreDir, StorePath)
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Types

instance Serialize Text where
  get = getText
  put = putText

instance Serialize BuildMode where
  get = getEnum
  put = putEnum

instance Serialize BuildStatus where
  get = getEnum
  put = putEnum

instance Serialize BuildResult where
  get = do
    status <- get
    errorMessage <-
      (\em -> Data.Bool.bool (Just em) Nothing (Data.Text.null em))
      <$> get
    timesBuilt <- getInt
    isNonDeterministic <- getBool
    startTime <- getTime
    stopTime <- getTime
    pure $ BuildResult{..}

  put BuildResult{..} = do
    put status
    case errorMessage of
      Just err -> putText err
      Nothing -> putText mempty
    putInt timesBuilt
    putBool isNonDeterministic
    putTime startTime
    putTime stopTime

getDerivation
  :: StoreDir
  -> Get (Derivation StorePath Text)
getDerivation storeDir = do
  outputs <-
    Data.Map.fromList
    <$> (getMany $ do
          outputName <- get
          path <- getPathOrFail storeDir
          hashAlgo <- get
          hash <- get
          pure (outputName, DerivationOutput{..})
        )

  -- Our type is Derivation, but in Nix
  -- the type sent over the wire is BasicDerivation
  -- which omits inputDrvs
  inputDrvs <- pure mempty
  inputSrcs <-
    Data.Set.fromList
    <$> getMany (getPathOrFail storeDir)

  platform <- get
  builder <- get
  args <-
    Data.Vector.fromList
    <$> getMany get

  env <-
    Data.Map.fromList
    <$> getMany ((,) <$> get <*> get)
  pure Derivation{..}

putDerivation :: StoreDir -> Putter (Derivation StorePath Text)
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

-- * Logger

instance Serialize Activity where
  get =
    toEnumCheckBounds . (+(-100)) <$> getInt
    >>= either fail pure
  put = putInt . (+100) . fromEnum

instance Serialize ActivityID where
  get = ActivityID <$> getInt
  put (ActivityID aid) = putInt aid

instance Serialize ActivityResult where
  get =
    toEnumCheckBounds . (+(-100)) <$> getInt
    >>= either fail pure
  put = putInt . (+100) . fromEnum

instance Serialize Field where
  get = (getInt :: Get Word8) >>= \case
    0 -> Field_LogInt <$> getInt
    1 -> Field_LogStr <$> getByteString
    x -> fail $ "Unknown log field type: " <> show x
  put (Field_LogInt x) = putInt (0 :: Word8) >> putInt x
  put (Field_LogStr x) = putInt (1 :: Word8) >> putByteString x

instance Serialize LoggerOpCode where
  get = getInt @Int >>= either fail pure . intToLoggerOpCode
  put = putInt @Int . loggerOpCodeToInt

instance Serialize Verbosity where
  get = getEnum
  put = putEnum
