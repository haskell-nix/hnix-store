{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description : NixSerializer
Copyright   : (c) John Ericson, 2023
                  Richard Marko, 2023
|-}

module System.Nix.Store.Remote.Serializer
  (
  -- * NixSerializer
    NixSerializer
  -- * Errors
  , PrimError(..)
  -- ** Runners
  , runSerialT
  , runG
  , runP
  -- * Primitives
  , int
  , bool
  , byteString
  , enum
  , text
  , maybeText
  -- * UTCTime
  , time
  -- * Combinators
  , list
  , set
  , hashSet
  , mapS
  -- * ProtoVersion
  , protoVersion
  -- * StorePath
  , storePath
  -- * Derivation
  , derivation
  -- * Build
  , buildMode
  , buildResult
  -- * Logger
  , LoggerError(..)
  , activityID
  , maybeActivity
  , activityResult
  , field
  , trace
  , basicError
  , errorInfo
  , loggerOpCode
  , logger
  , verbosity
  ) where

import Control.Monad.Except (MonadError, throwError, withExceptT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.Fixed (Uni)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Vector (Vector)
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)

import qualified Control.Monad
import qualified Control.Monad.Reader
import qualified Data.Bits
import qualified Data.HashSet
import qualified Data.Map.Strict
import qualified Data.Serialize.Get
import qualified Data.Serialize.Put
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Time.Clock.POSIX
import qualified Data.Vector

import Data.Serializer
import System.Nix.Build (BuildMode, BuildResult(..))
import System.Nix.Derivation (Derivation(..), DerivationOutput(..))
import System.Nix.StorePath (HasStoreDir(..), InvalidPathError, StorePath)
import System.Nix.Store.Remote.Serialize ()
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Types

-- | Transformer for @Serializer@
newtype SerialT r e m a = SerialT
  { _unSerialT :: ExceptT e (ReaderT r m) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError e
    , MonadReader r
    , MonadFail
    )

instance MonadTrans (SerialT r e) where
  lift = SerialT . lift . lift

-- | Runner for @SerialT@
runSerialT
  :: Monad m
  => r
  -> SerialT r e m a
  -> m (Either e a)
runSerialT r =
    (`runReaderT` r)
  . runExceptT
  . _unSerialT

mapError
  :: Functor m
  => (e -> e')
  -> SerialT r e m a
  -> SerialT r e' m a
mapError f =
  SerialT
  . withExceptT f
  . _unSerialT

-- * NixSerializer

type NixSerializer r e = Serializer (SerialT r e)

-- * Errors

data PrimError
  = PrimError
  | PrimError_BadPadding
      { badPaddingStr :: ByteString
      , badPaddingLen :: Int
      , badPaddingPads :: [Word8]
      }
  | PrimError_EnumOutOfMinBound Int
  | PrimError_EnumOutOfMaxBound Int
  | PrimError_IllegalBool Word64
  | PrimError_Path InvalidPathError
  deriving (Eq, Ord, Generic, Show)

-- ** Runners

runG
  :: NixSerializer r e a
  -> r
  -> ByteString
  -> Either (GetSerializerError e) a
runG serializer r =
    transformGetError
  . runGetS
      serializer
      (runSerialT r)

runP
  :: NixSerializer r e a
  -> r
  -> a
  -> Either e ByteString
runP serializer r =
    transformPutError
  . runPutS
      serializer
      (runSerialT r)

-- * Primitives

int :: Integral a => NixSerializer r e a
int = Serializer
  { getS = fromIntegral <$> lift Data.Serialize.Get.getWord64le
  , putS = lift . Data.Serialize.Put.putWord64le . fromIntegral
  }

bool :: NixSerializer r PrimError Bool
bool = Serializer
  { getS = getS (int @Word64) >>= \case
      0 -> pure False
      1 -> pure True
      x -> throwError $ PrimError_IllegalBool x
  , putS = lift . putBool
  }

byteString :: NixSerializer r PrimError ByteString
byteString = Serializer
  { getS = do
      len <- getS int
      st  <- lift $ Data.Serialize.Get.getByteString len
      Control.Monad.when (len `mod` 8 /= 0) $ do
        pads <- lift $ unpad $ fromIntegral $ 8 - (len `mod` 8)
        Control.Monad.unless
          (all (== 0) pads)
          $ throwError
          $ PrimError_BadPadding st len pads
      pure st
  , putS = lift . putByteString
  }
  where
    unpad x =
      Control.Monad.replicateM x Data.Serialize.Get.getWord8

-- | Utility toEnum version checking bounds using Bounded class
toEnumCheckBoundsM
  :: ( Enum a
     , MonadError PrimError m
     )
  => Int
  -> m a
toEnumCheckBoundsM = \case
  x | x < minBound -> throwError $ PrimError_EnumOutOfMinBound x
  x | x > maxBound -> throwError $ PrimError_EnumOutOfMaxBound x
  x | otherwise -> pure $ toEnum x

enum :: Enum a => NixSerializer r PrimError a
enum = Serializer
  { getS = getS int >>= toEnumCheckBoundsM
  , putS = lift . putEnum
  }

text :: NixSerializer r PrimError Text
text = mapIsoSerializer
  Data.Text.Encoding.decodeUtf8
  Data.Text.Encoding.encodeUtf8
  byteString

maybeText :: NixSerializer r PrimError (Maybe Text)
maybeText = mapIsoSerializer
  (\case
    t | Data.Text.null t -> Nothing
    t | otherwise -> Just t
  )
  (Prelude.maybe mempty id)
  text

-- * UTCTime

time :: NixSerializer r e UTCTime
time = Serializer
  { getS =
      Data.Time.Clock.POSIX.posixSecondsToUTCTime
      . toPicoSeconds
      <$> getS int
  , putS =
      putS int
      . fromPicoSeconds
      . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds
  }
  where
    -- fancy (*10^12), from Int to Uni to Pico(seconds)
    toPicoSeconds :: Int -> NominalDiffTime
    toPicoSeconds n = realToFrac (toEnum n :: Uni)

    -- fancy (`div`10^12), from Pico to Uni to Int
    fromPicoSeconds :: NominalDiffTime -> Int
    fromPicoSeconds = (fromEnum :: Uni -> Int) . realToFrac

-- * Combinators

list
  :: NixSerializer r e a
  -> NixSerializer r e [a]
list s = Serializer
  { getS = do
      count <- getS int
      Control.Monad.replicateM count (getS s)
  , putS = \xs -> do
      putS int (length xs)
      mapM_ (putS s) xs
  }

set
  :: Ord a
  => NixSerializer r e a
  -> NixSerializer r e (Set a)
set =
  mapIsoSerializer
    Data.Set.fromList
    Data.Set.toList
  . list

hashSet
  :: ( Eq a
     , Hashable a
     )
  => NixSerializer r e a
  -> NixSerializer r e (HashSet a)
hashSet =
  mapIsoSerializer
    Data.HashSet.fromList
    Data.HashSet.toList
  . list

mapS
  :: Ord k
  => NixSerializer r e k
  -> NixSerializer r e v
  -> NixSerializer r e (Map k v)
mapS k v =
  mapIsoSerializer
    Data.Map.Strict.fromList
    Data.Map.Strict.toList
  $ list
  $ tup k v

vector
  :: Ord a
  => NixSerializer r e a
  -> NixSerializer r e (Vector a)
vector =
  mapIsoSerializer
    Data.Vector.fromList
    Data.Vector.toList
  . list

-- * ProtoVersion

-- protoVersion_major & 0xFF00
-- protoVersion_minor & 0x00FF
protoVersion :: NixSerializer r e ProtoVersion
protoVersion = Serializer
  { getS = do
      v <- getS (int @Word32)
      pure ProtoVersion
        { protoVersion_major = fromIntegral $ Data.Bits.shiftR v 8
        , protoVersion_minor = fromIntegral $ v Data.Bits..&. 0x00FF
        }
  , putS = \p ->
      putS (int @Word32)
      $ ((Data.Bits.shiftL (fromIntegral $ protoVersion_major p :: Word32) 8)
          Data.Bits..|. fromIntegral (protoVersion_minor p))
  }

-- * StorePath

storePath :: HasStoreDir r => NixSerializer r PrimError StorePath
storePath = Serializer
  { getS = do
      sd <- Control.Monad.Reader.asks hasStoreDir
      lift (getPath sd)
      >>=
        either
          (throwError . PrimError_Path)
          pure
  , putS = \p -> do
      sd <- Control.Monad.Reader.asks hasStoreDir
      lift $ putPath sd p
  }

derivationOutput
  :: HasStoreDir r
  => NixSerializer r PrimError (DerivationOutput StorePath Text)
derivationOutput = Serializer
  { getS = do
      path <- getS storePath
      hashAlgo <- getS text
      hash <- getS text
      pure DerivationOutput{..}
  , putS = \DerivationOutput{..} -> do
      putS storePath path
      putS text hashAlgo
      putS text hash
  }

-- * Derivation

derivation
  :: HasStoreDir r
  => NixSerializer r PrimError (Derivation StorePath Text)
derivation = Serializer
  { getS = do
      outputs <- getS (mapS text derivationOutput)
      -- Our type is Derivation, but in Nix
      -- the type sent over the wire is BasicDerivation
      -- which omits inputDrvs
      inputDrvs <- pure mempty
      inputSrcs <- getS (set storePath)

      platform <- getS text
      builder <- getS text
      args <- getS (vector text)
      env <- getS (mapS text text)
      pure Derivation{..}
  , putS = \Derivation{..} -> do
      putS (mapS text derivationOutput) outputs
      putS (set storePath) inputSrcs
      putS text platform
      putS text builder
      putS (vector text) args
      putS (mapS text text) env
  }

-- * Build

buildMode :: NixSerializer r PrimError BuildMode
buildMode = enum

buildResult :: NixSerializer r PrimError BuildResult
buildResult = Serializer
  { getS = do
      status <- getS enum
      errorMessage <- getS maybeText
      timesBuilt <- getS int
      isNonDeterministic <- getS bool
      startTime <- getS time
      stopTime <- getS time
      pure $ BuildResult{..}

 , putS = \BuildResult{..} -> do
    putS enum status
    putS maybeText errorMessage
    putS int timesBuilt
    putS bool isNonDeterministic
    putS time startTime
    putS time stopTime
  }

-- * Logger

data LoggerError
  = LoggerError_Prim PrimError
  | LoggerError_InvalidOpCode Int
  | LoggerError_TooOldForErrorInfo
  | LoggerError_TooNewForBasicError
  | LoggerError_UnknownLogFieldType Word8
  deriving (Eq, Ord, Generic, Show)

mapPrimE
  :: Functor m
  => SerialT r PrimError m a
  -> SerialT r LoggerError m a
mapPrimE = mapError LoggerError_Prim

maybeActivity :: NixSerializer r LoggerError (Maybe Activity)
maybeActivity = Serializer
  { getS = getS (int @Int) >>= \case
      0 -> pure Nothing
      x -> mapPrimE $ toEnumCheckBoundsM (x - 100) >>= pure . Just
  , putS = \case
      Nothing -> putS (int @Int) 0
      Just act -> putS activity act
  }
  where
    activity :: NixSerializer r LoggerError Activity
    activity = Serializer
      { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
      , putS = putS int . (+100) . fromEnum
      }

activityID :: NixSerializer r LoggerError ActivityID
activityID = mapIsoSerializer ActivityID unActivityID int

activityResult :: NixSerializer r LoggerError ActivityResult
activityResult = Serializer
  { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
  , putS = putS int . (+100) . fromEnum
  }

field :: NixSerializer r LoggerError Field
field = Serializer
  { getS = getS (int @Word8) >>= \case
      0 -> Field_LogInt <$> getS int
      1 -> Field_LogStr <$> mapPrimE (getS text)
      x -> throwError $ LoggerError_UnknownLogFieldType x
  , putS = \case
      Field_LogInt x -> putS int (0 :: Word8) >> putS int x
      Field_LogStr x -> putS int (1 :: Word8) >> mapPrimE (putS text x)
  }

trace :: NixSerializer r LoggerError Trace
trace = Serializer
  { getS = do
      tracePosition <- (\case 0 -> Nothing; x -> Just x) <$> getS (int @Int)
      traceHint <- mapPrimE $ getS text
      pure Trace{..}
  , putS = \Trace{..} -> do
      maybe (putS (int @Int) 0) (putS int) $ tracePosition
      mapPrimE $ putS text traceHint
  }

basicError :: NixSerializer r LoggerError BasicError
basicError = Serializer
  { getS = do
      basicErrorMessage <- mapPrimE $ getS text
      basicErrorExitStatus <- getS int
      pure BasicError{..}

  , putS = \BasicError{..} -> do
      mapPrimE $ putS text basicErrorMessage
      putS int basicErrorExitStatus
  }

errorInfo :: NixSerializer r LoggerError ErrorInfo
errorInfo = Serializer
  { getS = do
      etyp <- mapPrimE $ getS text
      Control.Monad.unless (etyp == Data.Text.pack "Error")
        $ fail
        $ "get ErrorInfo: received unknown error type" ++ show etyp
      errorInfoLevel <- getS verbosity
      _name <- mapPrimE $ getS text -- removed error name
      errorInfoMessage <- mapPrimE $ getS text
      errorInfoPosition <- (\case 0 -> Nothing; x -> Just x) <$> getS int
      errorInfoTraces <- getS (list trace)
      pure ErrorInfo{..}

  , putS = \ErrorInfo{..} -> do
      mapPrimE $ do
        putS text $ Data.Text.pack "Error"
      putS verbosity errorInfoLevel
      mapPrimE $ do
        putS text $ Data.Text.pack "Error" -- removed error name
        putS text errorInfoMessage
        maybe (putS (int @Word8) 0) (putS int) errorInfoPosition
      putS (list trace) errorInfoTraces
  }

loggerOpCode :: NixSerializer r LoggerError LoggerOpCode
loggerOpCode = Serializer
  { getS = do
      c <- getS int
      either
        (pure $ throwError (LoggerError_InvalidOpCode c))
        pure
        $ intToLoggerOpCode c
  , putS = putS int . loggerOpCodeToInt
  }

logger
  :: HasProtoVersion r
  => NixSerializer r LoggerError Logger
logger = Serializer
  { getS = getS loggerOpCode >>= \case
      LoggerOpCode_Next ->
        mapPrimE $
          Logger_Next <$> getS text

      LoggerOpCode_Read ->
        Logger_Read <$> getS int

      LoggerOpCode_Write ->
        mapPrimE $
          Logger_Write <$> getS byteString

      LoggerOpCode_Last ->
        pure Logger_Last

      LoggerOpCode_Error -> do
        pv <- Control.Monad.Reader.asks hasProtoVersion
        Logger_Error <$>
          if protoVersion_minor pv >= 26
          then Right <$> getS errorInfo
          else Left <$> getS basicError

      LoggerOpCode_StartActivity -> do
        startActivityID <- getS activityID
        startActivityVerbosity <- getS verbosity
        startActivityType <- getS maybeActivity
        startActivityString <- mapPrimE $ getS byteString
        startActivityFields <- getS (list field)
        startActivityParentID <- getS activityID
        pure Logger_StartActivity{..}

      LoggerOpCode_StopActivity -> do
        stopActivityID <- getS activityID
        pure Logger_StopActivity{..}

      LoggerOpCode_Result -> do
        resultActivityID <- getS activityID
        resultType <- getS activityResult
        resultFields <- getS (list field)
        pure Logger_Result {..}

    , putS = \case
        Logger_Next s -> do
          putS loggerOpCode LoggerOpCode_Next
          mapError LoggerError_Prim $
            putS text s

        Logger_Read i -> do
          putS loggerOpCode LoggerOpCode_Read
          putS int i

        Logger_Write s -> do
          putS loggerOpCode LoggerOpCode_Write
          mapPrimE $ putS byteString s

        Logger_Last ->
          putS loggerOpCode LoggerOpCode_Last

        Logger_Error basicOrInfo -> do
          putS loggerOpCode LoggerOpCode_Error

          minor <- protoVersion_minor <$> Control.Monad.Reader.asks hasProtoVersion

          case basicOrInfo of
            Left _ | minor >= 26 -> throwError $ LoggerError_TooNewForBasicError
            Left e | otherwise -> putS basicError e
            Right _ | minor < 26 -> throwError $ LoggerError_TooOldForErrorInfo
            Right e -> putS errorInfo e

        Logger_StartActivity{..} -> do
          putS loggerOpCode LoggerOpCode_StartActivity
          putS activityID startActivityID
          putS verbosity startActivityVerbosity
          putS maybeActivity startActivityType
          mapPrimE $
            putS byteString startActivityString
          putS (list field) startActivityFields
          putS activityID startActivityParentID

        Logger_StopActivity{..} -> do
          putS loggerOpCode LoggerOpCode_StopActivity
          putS activityID stopActivityID

        Logger_Result{..} -> do
          putS loggerOpCode LoggerOpCode_Result
          putS activityID resultActivityID
          putS activityResult resultType
          putS (list field) resultFields
  }

verbosity :: NixSerializer r LoggerError Verbosity
verbosity = Serializer
  { getS = mapPrimE $ getS enum
  , putS = mapPrimE . putS enum
  }
