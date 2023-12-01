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
import Data.Word (Word8, Word32)

import qualified Control.Monad
import qualified Data.Bits
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

-- * BuildResult

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

-- * GCAction
--
instance Serialize GCAction where
  get = getEnum
  put = putEnum

-- * ProtoVersion

-- protoVersion_major & 0xFF00
-- protoVersion_minor & 0x00FF
instance Serialize ProtoVersion where
  get = do
    v <- getInt @Word32
    pure ProtoVersion
      { protoVersion_major = fromIntegral $ Data.Bits.shiftR v 8
      , protoVersion_minor = fromIntegral $ v Data.Bits..&. 0x00FF
      }
  put p =
    putInt @Word32
    $ ((Data.Bits.shiftL (fromIntegral $ protoVersion_major p :: Word32) 8)
        Data.Bits..|. fromIntegral (protoVersion_minor p))

-- * Derivation

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
    1 -> Field_LogStr <$> getText
    x -> fail $ "Unknown log field type: " <> show x
  put (Field_LogInt x) = putInt (0 :: Word8) >> putInt x
  put (Field_LogStr x) = putInt (1 :: Word8) >> putText x

instance Serialize Trace where
  get = do
    tracePosition <- (\case 0 -> Nothing; x -> Just x) <$> getInt @Int
    traceHint <- get
    pure Trace{..}
  put Trace{..} = do
    maybe (putInt @Int 0) putInt $ tracePosition
    put traceHint

instance Serialize BasicError where
  get = do
    basicErrorMessage <- get
    basicErrorExitStatus <- getInt
    pure BasicError{..}
  put BasicError{..} = do
    put basicErrorMessage
    putInt basicErrorExitStatus

instance Serialize ErrorInfo where
  get = do
    etyp <- get @Text
    Control.Monad.unless (etyp == Data.Text.pack "Error")
      $ fail
      $ "get ErrorInfo: received unknown error type" ++ show etyp
    errorInfoLevel <- get
    _name <- get @Text -- removed error name
    errorInfoMessage <- get
    errorInfoPosition <- (\case 0 -> Nothing; x -> Just x) <$> getInt @Int
    errorInfoTraces <- getMany get
    pure ErrorInfo{..}
  put ErrorInfo{..} = do
    put $ Data.Text.pack "Error"
    put errorInfoLevel
    put $ Data.Text.pack "Error" -- removed error name
    put errorInfoMessage
    maybe (putInt @Int 0) putInt $ errorInfoPosition
    putMany put errorInfoTraces

instance Serialize LoggerOpCode where
  get = getInt >>= either fail pure . word64ToLoggerOpCode
  put = putInt . loggerOpCodeToWord64

instance Serialize Verbosity where
  get = getEnum
  put = putEnum
