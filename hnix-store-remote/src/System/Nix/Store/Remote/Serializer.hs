{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Description : NixSerializer
Copyright   : (c) John Ericson, 2023
                  Sorki, 2023
|-}

module System.Nix.Store.Remote.Serializer
  (
  -- * NixSerializer
    NixSerializer
  , mapErrorS
  -- * Errors
  , SError(..)
  -- ** Runners
  , runExceptT
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
  , vector
  , json
  -- * ProtoVersion
  , protoVersion
  -- * StorePath
  , storePath
  , maybePath
  , storePathHashPart
  , storePathName
  -- * Metadata
  , pathMetadata
  -- * OutputName
  , outputName
  -- * Signatures
  , signature
  , narSignature
  -- * Some HashAlgo
  , someHashAlgo
  -- * Digest
  , digest
  -- * DSum HashAlgo Digest
  , namedDigest
  -- * Derivation
  , basicDerivation
  -- * Derivation
  , derivedPath
  -- * Build
  , buildMode
  -- * Logger
  , LoggerSError(..)
  , activityID
  , maybeActivity
  , activity
  , activityResult
  , field
  , trace
  , basicError
  , errorInfo
  , loggerOpCode
  , logger
  , verbosity
  -- * Handshake
  , HandshakeSError(..)
  , workerMagic
  , trustedFlag
  -- * Worker protocol
  , storeText
  , workerOp
  -- ** Request
  , RequestSError(..)
  , storeRequest
  -- ** Reply
  , ReplySError(..)
  , opSuccess
  , noop
  -- *** Realisation
  , buildTraceKeyTyped
  , realisation
  , realisationWithId
  -- *** BuildResult
  , buildResult
  -- *** GCResult
  , gcResult
  -- *** GCResult
  , gcRoot
  -- *** Missing
  , missing
  -- *** Maybe (Metadata StorePath)
  , maybePathMetadata
  ) where


import Control.Monad qualified
import Control.Monad.Except (MonadError, throwError, )
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Crypto.Hash (Digest, HashAlgorithm, SHA256)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified
import Data.Attoparsec.Text qualified
import Data.Bifunctor qualified
import Data.Bits qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified
import Data.ByteString.Char8 qualified
import Data.ByteString.Lazy qualified
import Data.Dependent.Sum (DSum((:=>)))
import Data.Fixed (Uni)
import Data.Functor.Identity
import Data.HashSet (HashSet)
import Data.HashSet qualified
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map.Strict qualified
import Data.Maybe qualified
import Data.Serialize.Get qualified
import Data.Serialize.Put qualified
import Data.Serializer
import Data.Set (Set)
import Data.Set qualified
import Data.Some (Some(Some))
import Data.Some qualified
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX qualified
import Data.Vector (Vector)
import Data.Vector qualified
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)
import System.Nix.Base (BaseEncoding(Base16, NixBase32))
import System.Nix.Base qualified
import System.Nix.Build (BuildMode, BuildResult(..), BuildSuccess(..), BuildFailure(..), BuildSuccessStatus(..), BuildFailureStatus(..))
import System.Nix.ContentAddress (ContentAddress)
import System.Nix.ContentAddress qualified
import System.Nix.Derivation.Traditional
import System.Nix.Derivation
import System.Nix.DerivedPath (DerivedPath(..), ParseOutputsError)
import System.Nix.DerivedPath qualified
import System.Nix.FileContentAddress (FileIngestionMethod(..))
import System.Nix.Hash (HashAlgo(..))
import System.Nix.Hash qualified
import System.Nix.JSON ()
import System.Nix.OutputName (OutputName)
import System.Nix.OutputName qualified
import System.Nix.Realisation (BuildTraceKeyError, Realisation(..), RealisationWithId(..))
import System.Nix.Realisation qualified
import System.Nix.Signature (Signature, NarSignature)
import System.Nix.Signature qualified
import System.Nix.Store.Remote.Types
import System.Nix.Store.Types (RepairMode(..))
import System.Nix.StorePath (StoreDir, InvalidNameError, InvalidPathError, StorePath, StorePathHashPart, StorePathName)
import System.Nix.StorePath qualified
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust(..))

mapErrorS
  :: (e -> e')
  -> NixSerializer e a
  -> NixSerializer e' a
mapErrorS f s = Serializer
  { getS = withExceptT f $ getS s
  , putS = putS s
  }

-- * NixSerializer

type NixSerializer e = Serializer (ExceptT e)

-- * Errors

data SError
  = SError
  | SError_BadPadding
      { badPaddingStr :: ByteString
      , badPaddingLen :: Int
      , badPaddingPads :: [Word8]
      }
  | SError_ContentAddress String
  | SError_DerivingPath
  | SError_DerivedPath ParseOutputsError
  | SError_BuildTraceKey BuildTraceKeyError
  | SError_Digest String
  | SError_EnumOutOfMinBound Int
  | SError_EnumOutOfMaxBound Int
  | SError_HashAlgo String
  | SError_IllegalBool Word64
  | SError_InvalidNixBase32
  | SError_JSONDecoding String
  -- | SError_NarHashMustBeSHA256
  | SError_NotYetImplemented String (ForPV ProtoVersion)
  | SError_Name InvalidNameError
  | SError_Path InvalidPathError
  | SError_Signature String
  | SError_DerivationOutputInvalidCombo Bool Bool Bool
  deriving (Eq, Ord, Generic, Show)

data ForPV a
  = ForPV_Newer a
  | ForPV_Older a
  deriving (Eq, Ord, Generic, Show)

-- ** Runners

runG
  :: NixSerializer e a
  -> ByteString
  -> Either (GetSerializerError e) a
runG serializer =
    transformGetError
  . runGetS
      serializer
      (runExceptT)

runP
  :: NixSerializer e a
  -> a
  -> ByteString
runP = runPutS

-- * Primitives

int :: Integral a => NixSerializer e a
int = Serializer
  { getS = fromIntegral <$> lift Data.Serialize.Get.getWord64le
  , putS = Data.Serialize.Put.putWord64le . fromIntegral
  }

bool :: NixSerializer SError Bool
bool = Serializer
  { getS = getS (int @Word64) >>= \case
      0 -> pure False
      1 -> pure True
      x -> throwError $ SError_IllegalBool x
  , putS = \case
      False -> putS (int @Word8) 0
      True  -> putS (int @Word8) 1
  }

byteString :: NixSerializer SError ByteString
byteString = Serializer
  { getS = do
      len <- getS int
      st  <- lift $ Data.Serialize.Get.getByteString len
      Control.Monad.when (len `mod` 8 /= 0) $ do
        pads <- unpad $ fromIntegral $ 8 - (len `mod` 8)
        Control.Monad.unless
          (all (== 0) pads)
          $ throwError
          $ SError_BadPadding st len pads
      pure st
  , putS = \x -> do
      let len = Data.ByteString.length x
      putS int len
      Data.Serialize.Put.putByteString x
      Control.Monad.when
        (len `mod` 8 /= 0)
        $ pad $ 8 - (len `mod` 8)
  }
  where
    unpad count =
      Control.Monad.replicateM
        count
        (lift Data.Serialize.Get.getWord8)
    pad count =
      Control.Monad.replicateM_
        count
        (Data.Serialize.Put.putWord8 0)

maybeByteString :: NixSerializer SError (Maybe ByteString)
maybeByteString = mapIsoSerializer
  (\case
    t | Data.ByteString.null t -> Nothing
    t | otherwise -> Just t
  )
  (Data.Maybe.fromMaybe mempty)
  byteString

-- | Utility toEnum version checking bounds using Bounded class
toEnumCheckBoundsM
  :: forall a m
   . ( Bounded a
     , Enum a
     , MonadError SError m
     )
  => Int
  -> m a
toEnumCheckBoundsM = \case
  x | x < fromEnum (minBound @a) -> throwError $ SError_EnumOutOfMinBound x
  x | x > fromEnum (maxBound @a) -> throwError $ SError_EnumOutOfMaxBound x
  x | otherwise -> pure $ toEnum x

enum
  :: ( Bounded a
     , Enum a
     )
  => NixSerializer SError a
enum = Serializer
  { getS = getS int >>= toEnumCheckBoundsM
  , putS = putS int . fromEnum
  }

text :: NixSerializer SError Text
text = mapIsoSerializer
  Data.Text.Encoding.decodeUtf8
  Data.Text.Encoding.encodeUtf8
  byteString

-- TODO Parser Builder
_textBuilder :: NixSerializer SError Builder
_textBuilder = Serializer
  { getS = Data.Text.Lazy.Builder.fromText <$> getS text
  , putS = putS text . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
  }

maybeText :: NixSerializer SError (Maybe Text)
maybeText = mapIsoSerializer
  (\case
    t | Data.Text.null t -> Nothing
    t | otherwise -> Just t
  )
  (Data.Maybe.fromMaybe mempty)
  text

-- * UTCTime

time :: NixSerializer e UTCTime
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
  :: NixSerializer e a
  -> NixSerializer e [a]
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
  => NixSerializer e a
  -> NixSerializer e (Set a)
set =
  mapIsoSerializer
    Data.Set.fromList
    Data.Set.toList
  . list

hashSet
  :: ( Eq a
     , Hashable a
     )
  => NixSerializer e a
  -> NixSerializer e (HashSet a)
hashSet =
  mapIsoSerializer
    Data.HashSet.fromList
    Data.HashSet.toList
  . list

mapS'
  :: Ord k
  => NixSerializer e (k, v)
  -> NixSerializer e (Map k v)
mapS' kv =
  mapIsoSerializer
    Data.Map.Strict.fromList
    Data.Map.Strict.toList
  $ list
  $ kv

mapS
  :: Ord k
  => NixSerializer e k
  -> NixSerializer e v
  -> NixSerializer e (Map k v)
mapS k v = mapS' $ tup k v

vector
  :: Ord a
  => NixSerializer e a
  -> NixSerializer e (Vector a)
vector =
  mapIsoSerializer
    Data.Vector.fromList
    Data.Vector.toList
  . list

json
  :: ( FromJSON a
     , ToJSON a
     )
  => NixSerializer SError a
json = mapPrismSerializer jsonP
    $ mapIsoSerializer
        Data.ByteString.Lazy.fromStrict
        Data.ByteString.Lazy.toStrict
        byteString

jsonP
  :: ( FromJSON a
     , ToJSON a
     )
  => AlmostPrism (ExceptT SError) Data.ByteString.Lazy.ByteString a
jsonP = AlmostPrism
    ( ExceptT
      . Identity
      . Data.Bifunctor.first SError_JSONDecoding
      . Data.Aeson.eitherDecode
    )
    Data.Aeson.encode

-- * ProtoVersion

-- protoVersion_major & 0xFF00
-- protoVersion_minor & 0x00FF
protoVersion :: NixSerializer e ProtoVersion
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

storePath :: StoreDir -> NixSerializer SError StorePath
storePath storeDir = mapPrismSerializer (storePathP storeDir) byteString

storePathP :: StoreDir -> AlmostPrism (ExceptT SError) ByteString StorePath
storePathP storeDir = AlmostPrism
  { _almostPrism_get =
      ExceptT
      . Identity
      . Data.Bifunctor.first SError_Path
      . System.Nix.StorePath.parsePath storeDir
  , _almostPrism_put = System.Nix.StorePath.storePathToRawFilePath storeDir
  }

maybePath
  :: StoreDir
  -> NixSerializer SError (Maybe StorePath)
maybePath storeDir = mapPrismSerializer (maybeAlmostPrism $ storePathP storeDir) maybeByteString

storePathHashPart :: NixSerializer SError StorePathHashPart
storePathHashPart =
  mapIsoSerializer
    System.Nix.StorePath.unsafeMakeStorePathHashPart
    System.Nix.StorePath.unStorePathHashPart
    $ mapPrismSerializer
        (AlmostPrism
          (ExceptT
           . Identity
           . Data.Bifunctor.first (pure SError_InvalidNixBase32)
           . System.Nix.Base.decodeWith NixBase32)
          (System.Nix.Base.encodeWith NixBase32)
        )
        text

storePathName :: NixSerializer SError StorePathName
storePathName =
  mapPrismSerializer
    (AlmostPrism
      (ExceptT
       . Identity
       . Data.Bifunctor.first SError_Name
       . System.Nix.StorePath.mkStorePathName)
      System.Nix.StorePath.unStorePathName
    )
    text

pathMetadata
  :: StoreDir
  -> NixSerializer SError (Metadata StorePath)
pathMetadata storeDir = Serializer
  { getS = do
      metadataDeriverPath <- getS $ maybePath storeDir

      digest' <- getS $ digest Base16
      let metadataNarHash = System.Nix.Hash.HashAlgo_SHA256 :=> digest'

      metadataReferences <- getS $ hashSet $ storePath storeDir
      metadataRegistrationTime <- getS time
      metadataNarBytes <-
        (\case
          0 -> Nothing
          size -> Just size
        ) <$> getS int
      metadataTrust <- getS storePathTrust

      metadataSigs <- getS $ set narSignature
      metadataContentAddress <- getS maybeContentAddress

      pure $ Metadata{..}

  , putS = \Metadata{..} -> do
      putS (maybePath storeDir) metadataDeriverPath

      let putNarHash
            :: DSum HashAlgo Digest
            -> PutM ()
          putNarHash = \case
            System.Nix.Hash.HashAlgo_SHA256 :=> d
              -> putS (digest @SHA256 Base16) d
            _ -> error "nar hash must be SHA 256"
                 -- throwError SError_NarHashMustBeSHA256

      putNarHash metadataNarHash

      putS (hashSet $ storePath storeDir) metadataReferences
      putS time metadataRegistrationTime
      putS int $ Data.Maybe.fromMaybe 0 metadataNarBytes
      putS storePathTrust metadataTrust
      putS (set narSignature) metadataSigs
      putS maybeContentAddress metadataContentAddress
  }
  where
    maybeContentAddress
      :: NixSerializer SError (Maybe ContentAddress)
    maybeContentAddress =
      mapPrismSerializer
        (maybeAlmostPrism $ AlmostPrism
          (ExceptT
           . Identity
           . Data.Bifunctor.first SError_ContentAddress
           . System.Nix.ContentAddress.parseContentAddress
          )
          System.Nix.ContentAddress.buildContentAddress
        )
        maybeText

    storePathTrust
      :: NixSerializer SError StorePathTrust
    storePathTrust =
      mapIsoSerializer
        (\case False -> BuiltElsewhere; True -> BuiltLocally)
        (\case BuiltElsewhere -> False; BuiltLocally -> True)
        bool

-- * OutputName

outputName :: NixSerializer SError OutputName
outputName =
  mapIsoSerializer
    System.Nix.OutputName.OutputName
     System.Nix.OutputName.unOutputName
    storePathName

-- * Signatures

signature
  :: NixSerializer SError Signature
signature =
  mapPrismSerializer
    (AlmostPrism
      (ExceptT
       . Identity
       . Data.Bifunctor.first SError_Signature
       . Data.Attoparsec.Text.parseOnly
           System.Nix.Signature.signatureParser)
      (System.Nix.Signature.signatureToText)
    )
    text

narSignature
  :: NixSerializer SError NarSignature
narSignature =
  mapPrismSerializer
    (AlmostPrism
      (ExceptT
       . Identity
       . Data.Bifunctor.first SError_Signature
       . Data.Attoparsec.Text.parseOnly
           System.Nix.Signature.narSignatureParser)
      (System.Nix.Signature.narSignatureToText)
    )
    text

-- * Some HashAlgo

someHashAlgo :: NixSerializer SError (Some HashAlgo)
someHashAlgo =
  mapPrismSerializer
    (AlmostPrism
      (ExceptT
       . Identity
       . Data.Bifunctor.first SError_HashAlgo
       . System.Nix.Hash.textToAlgo)
      (Data.Some.foldSome System.Nix.Hash.algoToText)
    )
    text

-- * Digest

digest
  :: forall a
   . HashAlgorithm a
  => BaseEncoding
  -> NixSerializer SError (Digest a)
digest base = mapPrismSerializer (digestP base) $ text

digestP
  :: forall a
  . HashAlgorithm a
  => BaseEncoding
  -> AlmostPrism (ExceptT SError) Text (Digest a)
digestP base = AlmostPrism
  (ExceptT
   . Identity
   . Data.Bifunctor.first SError_Digest
   . System.Nix.Hash.decodeDigestWith @a base)
  (System.Nix.Hash.encodeDigestWith base)

-- * DSum HashAlgo Digest

namedDigest :: NixSerializer SError (DSum HashAlgo Digest)
namedDigest = Serializer
  { getS = do
      sriHash <- getS text
      let (sriName, _h) = Data.Text.breakOn (Data.Text.singleton '-') sriHash
      -- bit hacky since mkNamedDigest does the check
      -- that the expected matches but we don't know
      -- what we expect here (i.e. handle each HashAlgo)
      case System.Nix.Hash.mkNamedDigest sriName sriHash of
        Left e -> throwError $ SError_Digest e
        Right x -> pure x
  -- TODO: we also lack a builder for SRI hashes
  -- , putS = putS textBuilder . System.Nix.Hash.algoDigestBuilder
  , putS = \(algo :=> d) -> do
      putS text
        $  System.Nix.Hash.algoToText algo
        <> (Data.Text.singleton '-')
        <> System.Nix.Hash.encodeDigestWith NixBase32 d
  }

derivationOutput
  :: StoreDir
  -> NixSerializer SError FreeformDerivationOutput
derivationOutput storeDir = Serializer
  { getS = do
      rawPath <- getS text
      rawMethodHashAlgo <- getS text
      rawHash <- getS text
      parseRawDerivationOutput storeDir $ RawDerivationOutput {..}
  , putS = \output -> do
      let RawDerivationOutput {..} = renderRawDerivationOutput storeDir output
      putS text rawPath
      putS text rawMethodHashAlgo
      putS text rawHash
  }

-- * Derivation

basicDerivation
  :: StoreDir
  -> NixSerializer SError (TraditionalDerivation' (Set StorePath) FreeformDerivationOutputs)
basicDerivation storeDir = Serializer
  { getS = do
      anonOutputs <- getS $ mapS' $ tup outputName $ derivationOutput storeDir
      anonInputs <- getS $ set $ storePath storeDir
      anonPlatform <- getS text
      anonBuilder <- getS text
      anonArgs <- getS $ vector text
      anonEnv <- getS $ mapS text text
      pure $ TraditionalDerivation{..}
  , putS = \TraditionalDerivation{..} -> do
      putS (mapS' $ tup outputName $ derivationOutput storeDir) anonOutputs
      putS (set $ storePath storeDir) anonInputs
      putS text anonPlatform
      putS text anonBuilder
      putS (vector text) anonArgs
      putS (mapS text text) anonEnv
  }

-- * DerivedPath

derivedPathNew
  :: StoreDir
  -> NixSerializer SError DerivedPath
derivedPathNew storeDir = Serializer
  { getS = do
      p <- getS text
      case System.Nix.DerivedPath.parseDerivedPath storeDir p of
        Left err -> throwError $ SError_DerivedPath err
        Right x -> pure x
  , putS = \d -> do
      putS text (System.Nix.DerivedPath.derivedPathToText storeDir d)
  }

derivedPath
  :: StoreDir
  -> ProtoVersion
  -> NixSerializer SError DerivedPath
derivedPath storeDir pv = Serializer
  { getS =
      if pv < ProtoVersion 1 30
        then DerivedPath_Opaque <$> getS (storePath storeDir)
        else getS $ derivedPathNew storeDir
  , putS = \d ->
      if pv < ProtoVersion 1 30
        then case d of
          DerivedPath_Opaque p -> putS (storePath storeDir) p
          _ -> error "not yet implemented"
          --     throwError
          --      $ SError_NotYetImplemented
          --          "DerivedPath_Built"
          --          (ForPV_Older pv)
        else putS (derivedPathNew storeDir) d
  }

-- * Build

buildMode :: NixSerializer SError BuildMode
buildMode = enum

-- * Logger

data LoggerSError
  = LoggerSError_Prim SError
  | LoggerSError_InvalidOpCode Word64
  | LoggerSError_TooOldForErrorInfo
  | LoggerSError_TooNewForBasicError
  | LoggerSError_UnknownLogFieldType Word8
  deriving (Eq, Ord, Generic, Show)

mapPrimE
  :: Functor m
  => ExceptT SError m a
  -> ExceptT LoggerSError m a
mapPrimE = withExceptT LoggerSError_Prim

maybeActivity :: NixSerializer LoggerSError (Maybe Activity)
maybeActivity = Serializer
  { getS = getS (int @Int) >>= \case
      0 -> pure Nothing
      x -> mapPrimE $ toEnumCheckBoundsM (x - 100) >>= pure . Just
  , putS = \case
      Nothing -> putS (int @Int) 0
      Just act -> putS activity act
  }

activity :: NixSerializer LoggerSError Activity
activity = Serializer
  { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
  , putS = putS int . (+100) . fromEnum
  }

activityID :: NixSerializer LoggerSError ActivityID
activityID = mapIsoSerializer ActivityID unActivityID int

activityResult :: NixSerializer LoggerSError ActivityResult
activityResult = Serializer
  { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
  , putS = putS int . (+100) . fromEnum
  }

field :: NixSerializer LoggerSError Field
field = Serializer
  { getS = getS (int @Word8) >>= \case
      0 -> Field_LogInt <$> getS int
      1 -> Field_LogStr <$> mapPrimE (getS text)
      x -> throwError $ LoggerSError_UnknownLogFieldType x
  , putS = \case
      Field_LogInt x -> putS int (0 :: Word8) >> putS int x
      Field_LogStr x -> putS int (1 :: Word8) >> putS text x
  }

trace :: NixSerializer LoggerSError Trace
trace = Serializer
  { getS = do
      tracePosition <- (\case 0 -> Nothing; x -> Just x) <$> getS (int @Int)
      traceHint <- mapPrimE $ getS text
      pure Trace{..}
  , putS = \Trace{..} -> do
      putS int $ Data.Maybe.fromMaybe 0 tracePosition
      putS text traceHint
  }

basicError :: NixSerializer LoggerSError BasicError
basicError = Serializer
  { getS = do
      basicErrorMessage <- mapPrimE $ getS text
      basicErrorExitStatus <- getS int
      pure BasicError{..}

  , putS = \BasicError{..} -> do
      putS text basicErrorMessage
      putS int basicErrorExitStatus
  }

errorInfo :: NixSerializer LoggerSError ErrorInfo
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
      do
        putS text $ Data.Text.pack "Error"
      putS verbosity errorInfoLevel
      do
        putS text $ Data.Text.pack "Error" -- removed error name
        putS text errorInfoMessage
        putS int $ Data.Maybe.fromMaybe 0 errorInfoPosition
      putS (list trace) errorInfoTraces
  }

loggerOpCode :: NixSerializer LoggerSError LoggerOpCode
loggerOpCode = Serializer
  { getS = do
      c <- getS int
      either
        (pure $ throwError (LoggerSError_InvalidOpCode c))
        pure
        $ word64ToLoggerOpCode c
  , putS = putS int . loggerOpCodeToWord64
  }

logger
  :: ProtoVersion
  -> NixSerializer LoggerSError Logger
logger pv = Serializer
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
          putS text s

        Logger_Read i -> do
          putS loggerOpCode LoggerOpCode_Read
          putS int i

        Logger_Write s -> do
          putS loggerOpCode LoggerOpCode_Write
          putS byteString s

        Logger_Last ->
          putS loggerOpCode LoggerOpCode_Last

        Logger_Error basicOrInfo -> do
          putS loggerOpCode LoggerOpCode_Error

          let minor = protoVersion_minor pv

          case basicOrInfo of
            Left _ | minor >= 26 -> error "protocol too new" -- throwError $ LoggerSError_TooNewForBasicError
            Left e | otherwise -> putS basicError e
            Right _ | minor < 26 -> error "protocol too old" -- throwError $ LoggerSError_TooOldForErrorInfo
            Right e -> putS errorInfo e

        Logger_StartActivity{..} -> do
          putS loggerOpCode LoggerOpCode_StartActivity
          putS activityID startActivityID
          putS verbosity startActivityVerbosity
          putS maybeActivity startActivityType
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

verbosity :: NixSerializer LoggerSError Verbosity
verbosity = Serializer
  { getS = mapPrimE $ getS enum
  , putS = putS enum
  }

-- * Handshake

data HandshakeSError
  = HandshakeSError_InvalidWorkerMagic Word64
  | HandshakeSError_InvalidTrustedFlag Word8
  deriving (Eq, Ord, Generic, Show)

workerMagic :: NixSerializer HandshakeSError WorkerMagic
workerMagic = Serializer
  { getS = do
      c <- getS int
      either
        (pure $ throwError (HandshakeSError_InvalidWorkerMagic c))
        pure
        $ word64ToWorkerMagic c
  , putS = putS int . workerMagicToWord64
  }

trustedFlag :: NixSerializer HandshakeSError (Maybe TrustedFlag)
trustedFlag = Serializer
  { getS = do
      n :: Word8 <- getS int
      case n of
        0 -> return $ Nothing
        1 -> return $ Just TrustedFlag_Trusted
        2 -> return $ Just TrustedFlag_NotTrusted
        _ -> throwError (HandshakeSError_InvalidTrustedFlag n)
  , putS = \n -> putS int $ case n of
      Nothing -> 0 :: Word8
      Just TrustedFlag_Trusted -> 1
      Just TrustedFlag_NotTrusted -> 2
  }

-- * Worker protocol

storeText :: NixSerializer SError StoreText
storeText = Serializer
  { getS = do
      storeTextName <- getS storePathName
      storeTextText <- getS text
      pure StoreText{..}
  , putS = \StoreText{..} -> do
      putS storePathName storeTextName
      putS text storeTextText
  }

workerOp :: NixSerializer SError WorkerOp
workerOp = enum

-- * Request

data RequestSError
  = RequestSError_NotYetImplemented WorkerOp
  | RequestSError_ReservedOp WorkerOp
  | RequestSError_PrimGet SError
  | RequestSError_PrimWorkerOp SError
  deriving (Eq, Ord, Generic, Show)

storeRequest
  :: StoreDir
  -> ProtoVersion
  -> NixSerializer RequestSError (Some StoreRequest)
storeRequest storeDir pv = Serializer
  { getS = withExceptT RequestSError_PrimWorkerOp (getS workerOp) >>= \case
      WorkerOp_AddToStore -> mapGetE $ do
        pathName <- getS storePathName
        _fixed <- getS bool -- obsolete
        recursive <- getS enum
        hashAlgo <- getS someHashAlgo

        -- not supported by ProtoVersion < 1.25
        let repair = RepairMode_DontRepair

        pure $ Some (AddToStore pathName recursive hashAlgo repair)

      WorkerOp_AddToStoreNar -> mapGetE $ do
        storePath' <- getS $ storePath storeDir
        metadata <- getS $ pathMetadata storeDir
        repair <- getS bool
        let repairMode = if repair then RepairMode_DoRepair else RepairMode_DontRepair
        dontCheckSigs <- getS bool
        let checkSigs = if dontCheckSigs then CheckMode_DontCheck else CheckMode_DoCheck

        pure $ Some (AddToStoreNar storePath' metadata repairMode checkSigs)

      WorkerOp_AddTextToStore -> mapGetE $ do
        txt <- getS storeText
        paths <- getS $ hashSet $ storePath storeDir
        let repair = RepairMode_DontRepair
        pure $ Some (AddTextToStore txt paths repair)

      WorkerOp_AddSignatures -> mapGetE $ do
        path <- getS $ storePath storeDir
        signatures <- getS (set signature)
        pure $ Some (AddSignatures path signatures)

      WorkerOp_AddIndirectRoot -> mapGetE $ do
        Some . AddIndirectRoot <$> getS (storePath storeDir)

      WorkerOp_AddTempRoot -> mapGetE $ do
        Some . AddTempRoot <$> getS (storePath storeDir)

      WorkerOp_BuildPaths -> mapGetE $ do
        derived <- getS (set $ derivedPath storeDir pv)
        buildMode' <- getS buildMode
        pure $ Some (BuildPaths derived buildMode')

      WorkerOp_BuildDerivation -> mapGetE $ do
        path <- getS $ storePath storeDir
        let name = System.Nix.StorePath.storePathName path
        drv0 <- getS $ basicDerivation storeDir
        let drv1 = withName name drv0
        outputs <- toSpecificOutputs storeDir name $ outputs drv1
        let drv2 = drv1 { outputs = outputs }
        buildMode' <- getS buildMode
        pure $ Some (BuildDerivation path drv2 buildMode')

      WorkerOp_CollectGarbage -> mapGetE $ do
        gcOptionsOperation <- getS enum
        gcOptionsPathsToDelete <- getS (hashSet $ storePath storeDir)
        gcOptionsIgnoreLiveness <- getS bool
        gcOptionsMaxFreed <- getS int
        -- obsolete fields
        Control.Monad.forM_ [0..(2 :: Word8)]
          $ pure $ getS (int @Word8)
        pure $ Some (CollectGarbage GCOptions{..})

      WorkerOp_EnsurePath -> mapGetE $ do
        Some . EnsurePath <$> getS (storePath storeDir)

      WorkerOp_FindRoots -> mapGetE $ do
        pure $ Some FindRoots

      WorkerOp_IsValidPath -> mapGetE $ do
        Some . IsValidPath <$> getS (storePath storeDir)

      WorkerOp_NarFromPath -> mapGetE $ do
        Some . NarFromPath <$> getS (storePath storeDir)

      WorkerOp_QueryValidPaths -> mapGetE $ do
        paths <- getS (hashSet $ storePath storeDir)
        substituteMode <- getS enum
        pure $ Some (QueryValidPaths paths substituteMode)

      WorkerOp_QueryAllValidPaths -> mapGetE $ do
        pure $ Some QueryAllValidPaths

      WorkerOp_QuerySubstitutablePaths -> mapGetE $ do
        Some . QuerySubstitutablePaths <$> getS (hashSet $ storePath storeDir)

      WorkerOp_QueryPathInfo -> mapGetE $ do
        Some . QueryPathInfo <$> getS (storePath storeDir)

      WorkerOp_QueryReferrers -> mapGetE $ do
        Some . QueryReferrers <$> getS (storePath storeDir)

      WorkerOp_QueryValidDerivers -> mapGetE $ do
        Some . QueryValidDerivers <$> getS (storePath storeDir)

      WorkerOp_QueryDerivationOutputs -> mapGetE $ do
        Some . QueryDerivationOutputs <$> getS (storePath storeDir)

      WorkerOp_QueryDerivationOutputNames -> mapGetE $ do
        Some . QueryDerivationOutputNames <$> getS (storePath storeDir)

      WorkerOp_QueryPathFromHashPart -> mapGetE $ do
        Some . QueryPathFromHashPart <$> getS storePathHashPart

      WorkerOp_QueryMissing -> mapGetE $ do
        Some . QueryMissing <$> getS (set $ derivedPath storeDir pv)

      WorkerOp_OptimiseStore -> mapGetE $ do
        pure $ Some OptimiseStore

      WorkerOp_SyncWithGC -> mapGetE $ do
        pure $ Some SyncWithGC

      WorkerOp_VerifyStore -> mapGetE $ do
        checkMode <- getS enum
        repairMode <- getS enum

        pure $ Some (VerifyStore checkMode repairMode)

      w@WorkerOp_Reserved_0__ -> reserved w
      w@WorkerOp_Reserved_2__ -> reserved w
      w@WorkerOp_Reserved_15__ -> reserved w
      w@WorkerOp_Reserved_17__ -> reserved w

      w@WorkerOp_AddBuildLog -> notYet w
      w@WorkerOp_AddMultipleToStore -> notYet w
      w@WorkerOp_BuildPathsWithResults -> notYet w
      w@WorkerOp_ClearFailedPaths -> notYet w
      w@WorkerOp_ExportPath -> notYet w
      w@WorkerOp_HasSubstitutes -> notYet w
      w@WorkerOp_ImportPaths -> notYet w
      w@WorkerOp_QueryDerivationOutputMap -> notYet w
      w@WorkerOp_QueryDeriver -> notYet w
      w@WorkerOp_QueryFailedPaths -> notYet w
      w@WorkerOp_QueryPathHash -> notYet w
      w@WorkerOp_QueryRealisation -> notYet w
      w@WorkerOp_QuerySubstitutablePathInfo -> notYet w
      w@WorkerOp_QuerySubstitutablePathInfos -> notYet w
      w@WorkerOp_QueryReferences -> notYet w
      w@WorkerOp_RegisterDrvOutput -> notYet w
      w@WorkerOp_SetOptions -> notYet w

  , putS = \case
      Some (AddToStore pathName recursive hashAlgo _repair) -> do
        putS workerOp WorkerOp_AddToStore

        putS storePathName pathName
        -- obsolete fixed
        putS bool
          $ not
          $ hashAlgo == Some HashAlgo_SHA256
            && (recursive == FileIngestionMethod_NixArchive)

        putS bool (recursive == FileIngestionMethod_NixArchive)
        putS someHashAlgo hashAlgo

      Some (AddToStoreNar storePath' metadata repair checkSigs) -> do
        putS workerOp WorkerOp_AddToStoreNar

        putS (storePath storeDir) storePath'
        putS (pathMetadata storeDir) metadata
        putS bool $ repair == RepairMode_DoRepair
        putS bool $ checkSigs == CheckMode_DontCheck

      Some (AddTextToStore txt paths _repair) -> do
        putS workerOp WorkerOp_AddTextToStore

        putS storeText txt
        putS (hashSet $ storePath storeDir) paths

      Some (AddSignatures path signatures) -> do
        putS workerOp WorkerOp_AddSignatures

        putS (storePath storeDir) path
        putS (set signature) signatures

      Some (AddIndirectRoot path) -> do
        putS workerOp WorkerOp_AddIndirectRoot
        putS (storePath storeDir) path

      Some (AddTempRoot path) -> do
        putS workerOp WorkerOp_AddTempRoot
        putS (storePath storeDir) path

      Some (BuildPaths derived buildMode') -> do
        putS workerOp WorkerOp_BuildPaths

        putS (set $ derivedPath storeDir pv) derived
        putS buildMode buildMode'

      Some (BuildDerivation path drv0 buildMode') -> do
        putS workerOp WorkerOp_BuildDerivation

        putS (storePath storeDir) path
        let drv1 = drv0 { outputs = fromSpecificOutputs storeDir (name drv0) $ outputs drv0 }
        let drv2 = withoutName drv1
        putS (basicDerivation storeDir) drv2
        putS buildMode buildMode'

      Some (CollectGarbage GCOptions{..}) -> do
        putS workerOp WorkerOp_CollectGarbage

        putS enum gcOptionsOperation
        putS (hashSet $ storePath storeDir) gcOptionsPathsToDelete
        putS bool gcOptionsIgnoreLiveness
        putS int gcOptionsMaxFreed
        -- obsolete fields
        Control.Monad.forM_ [0..(2 :: Word8)]
          $ pure $ putS int (0 :: Word8)

      Some (EnsurePath path) -> do
        putS workerOp WorkerOp_EnsurePath
        putS (storePath storeDir) path

      Some FindRoots -> do
        putS workerOp WorkerOp_FindRoots

      Some (IsValidPath path) -> do
        putS workerOp WorkerOp_IsValidPath
        putS (storePath storeDir) path

      Some (NarFromPath path) -> do
        putS workerOp WorkerOp_NarFromPath
        putS (storePath storeDir) path

      Some (QueryValidPaths paths substituteMode) -> do
        putS workerOp WorkerOp_QueryValidPaths

        putS (hashSet $ storePath storeDir) paths
        putS enum substituteMode

      Some QueryAllValidPaths -> do
        putS workerOp WorkerOp_QueryAllValidPaths

      Some (QuerySubstitutablePaths paths) -> do
        putS workerOp WorkerOp_QuerySubstitutablePaths
        putS (hashSet $ storePath storeDir) paths

      Some (QueryPathInfo path) -> do
        putS workerOp WorkerOp_QueryPathInfo
        putS (storePath storeDir) path

      Some (QueryReferrers path) -> do
        putS workerOp WorkerOp_QueryReferrers
        putS (storePath storeDir) path

      Some (QueryValidDerivers path) -> do
        putS workerOp WorkerOp_QueryValidDerivers
        putS (storePath storeDir) path

      Some (QueryDerivationOutputs path) -> do
        putS workerOp WorkerOp_QueryDerivationOutputs
        putS (storePath storeDir) path

      Some (QueryDerivationOutputNames path) -> do
        putS workerOp WorkerOp_QueryDerivationOutputNames
        putS (storePath storeDir) path

      Some (QueryPathFromHashPart pathHashPart) -> do
        putS workerOp WorkerOp_QueryPathFromHashPart
        putS storePathHashPart pathHashPart

      Some (QueryMissing derived) -> do
        putS workerOp WorkerOp_QueryMissing
        putS (set $ derivedPath storeDir pv) derived

      Some OptimiseStore -> do
        putS workerOp WorkerOp_OptimiseStore

      Some SyncWithGC -> do
        putS workerOp WorkerOp_SyncWithGC

      Some (VerifyStore checkMode repairMode) -> do
        putS workerOp WorkerOp_VerifyStore
        putS enum checkMode
        putS enum repairMode
  }
  where
    mapGetE
      :: Functor m
      => ExceptT SError m a
      -> ExceptT RequestSError m a
    mapGetE = withExceptT RequestSError_PrimGet

    notYet
      :: MonadError RequestSError m
      => WorkerOp
      -> m a
    notYet = throwError . RequestSError_NotYetImplemented

    reserved
      :: MonadError RequestSError m
      => WorkerOp
      -> m a
    reserved = throwError . RequestSError_ReservedOp

-- ** Reply

data ReplySError
  = ReplySError_PrimGet SError
  | ReplySError_BuildTraceKey SError
  | ReplySError_GCResult SError
  | ReplySError_Metadata SError
  | ReplySError_Missing SError
  | ReplySError_Realisation SError
  | ReplySError_RealisationWithId SError
  | ReplySError_UnexpectedFalseOpSuccess
  deriving (Eq, Ord, Generic, Show)

mapGetER
  :: Functor m
  => ExceptT SError m a
  -> ExceptT ReplySError m a
mapGetER = withExceptT ReplySError_PrimGet

-- | Parse a bool returned at the end of simple operations.
-- This is always 1 (@True@) so we assert that it really is so.
-- Errors for these operations are indicated via @Logger_Error@.
opSuccess :: NixSerializer ReplySError SuccessCodeReply
opSuccess = Serializer
  { getS = do
      retCode <- mapGetER $ getS bool
      Control.Monad.unless
        (retCode == True)
        $ throwError ReplySError_UnexpectedFalseOpSuccess
      pure SuccessCodeReply
  , putS = \_ -> putS bool True
  }

noop :: a -> NixSerializer ReplySError a
noop ret = Serializer
  { getS = pure ret
  , putS = \_ -> pure ()
  }

-- *** Realisation

buildTraceKeyTyped :: NixSerializer ReplySError (System.Nix.Realisation.BuildTraceKey OutputName)
buildTraceKeyTyped = mapErrorS ReplySError_BuildTraceKey $
  mapPrismSerializer
    AlmostPrism
    { _almostPrism_get =
      ExceptT
      . Identity
      . Data.Bifunctor.first SError_BuildTraceKey
      . System.Nix.Realisation.buildTraceKeyParser
          System.Nix.OutputName.mkOutputName
    , _almostPrism_put =
      Data.Text.Lazy.toStrict
      . Data.Text.Lazy.Builder.toLazyText
      . System.Nix.Realisation.buildTraceKeyBuilder
          (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)
    }
    text

realisation :: NixSerializer ReplySError Realisation
realisation = mapErrorS ReplySError_Realisation json

realisationWithId :: NixSerializer ReplySError RealisationWithId
realisationWithId = mapErrorS ReplySError_RealisationWithId json

-- *** BuildResult

buildResult
  :: StoreDir
  -> ProtoVersion
  -> NixSerializer ReplySError BuildResult
buildResult _storeDir pv = Serializer
  { getS = do
      statusWord <- mapGetER $ getS enum
      errorMessage <- mapGetER $ getS maybeText

      ( buildResultTimesBuilt
        , isNonDeterministic
        , buildResultStartTime
        , buildResultStopTime
        ) <-
        if protoVersion_minor pv >= 29
        then mapGetER $ do
          tb <- getS int
          nondet <- getS bool
          start <- (\case x | x == t0 -> Nothing; x -> Just x) <$> getS time
          end <- (\case x | x == t0 -> Nothing; x -> Just x) <$> getS time
          pure $ (tb, nondet, start, end)
        else pure $ (0, False, Nothing, Nothing)

      parsedBuiltOutputs <-
        if protoVersion_minor pv >= 28
        then do
          wireMap <- getS (mapS buildTraceKeyTyped realisationWithId)
          pure
            $ Data.Map.Strict.fromList
            $ map (\(_, RealisationWithId (a, b)) -> (a, b))
            $ Data.Map.Strict.toList wireMap
        else pure mempty

      let buildResultStatus = case (wireToStatus statusWord, errorMessage) of
            (Right successStatus, _) -> Right $ BuildSuccess successStatus parsedBuiltOutputs
            (Left failureStatus, Just errorMsg) -> Left $ BuildFailure failureStatus errorMsg isNonDeterministic
            (Left failureStatus, Nothing) -> Left $ BuildFailure failureStatus Data.Text.empty isNonDeterministic

      let buildResultStartTime' = Data.Maybe.fromMaybe t0 buildResultStartTime
          buildResultStopTime' = Data.Maybe.fromMaybe t0 buildResultStopTime
          buildResultCpuUser = Nothing
          buildResultCpuSystem = Nothing

      pure BuildResult
        { buildResultStatus = buildResultStatus
        , buildResultTimesBuilt = buildResultTimesBuilt
        , buildResultStartTime = buildResultStartTime'
        , buildResultStopTime = buildResultStopTime'
        , buildResultCpuUser = buildResultCpuUser
        , buildResultCpuSystem = buildResultCpuSystem
        }

  , putS = \BuildResult{..} -> do
      let (statusWord, errorMessage, isNonDeterministic, builtOutputs) = case buildResultStatus of
            Right (BuildSuccess st bo) -> (successStatusToWire st, Nothing, False, bo)
            Left (BuildFailure st em nd) -> (failureStatusToWire st, Just em, nd, mempty)

      putS enum statusWord
      putS maybeText errorMessage
      Control.Monad.when (protoVersion_minor pv >= 29) $ do
        putS int buildResultTimesBuilt
        putS bool isNonDeterministic
        putS time buildResultStartTime
        putS time buildResultStopTime
      Control.Monad.when (protoVersion_minor pv >= 28)
        $ putS (mapS buildTraceKeyTyped realisationWithId)
        $ Data.Map.Strict.fromList
        $ map (\(a, b) -> (a, RealisationWithId (a, b)))
        $ Data.Map.Strict.toList
        $ builtOutputs
  }
  where
    t0 :: UTCTime
    t0 = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0

    -- Convert wire format status code to either success or failure status
    wireToStatus :: Word8 -> Either BuildFailureStatus BuildSuccessStatus
    wireToStatus 0 = Right BuildSuccessStatus_Built
    wireToStatus 1 = Right BuildSuccessStatus_Substituted
    wireToStatus 2 = Right BuildSuccessStatus_AlreadyValid
    wireToStatus 3 = Left BuildFailureStatus_PermanentFailure
    wireToStatus 4 = Left BuildFailureStatus_InputRejected
    wireToStatus 5 = Left BuildFailureStatus_OutputRejected
    wireToStatus 6 = Left BuildFailureStatus_TransientFailure
    wireToStatus 7 = Left BuildFailureStatus_CachedFailure
    wireToStatus 8 = Left BuildFailureStatus_TimedOut
    wireToStatus 9 = Left BuildFailureStatus_MiscFailure
    wireToStatus 10 = Left BuildFailureStatus_DependencyFailed
    wireToStatus 11 = Left BuildFailureStatus_LogLimitExceeded
    wireToStatus 12 = Left BuildFailureStatus_NotDeterministic
    wireToStatus 13 = Right BuildSuccessStatus_ResolvesToAlreadyValid
    wireToStatus 14 = Left BuildFailureStatus_NoSubstituters
    wireToStatus 15 = Left BuildFailureStatus_HashMismatch
    wireToStatus _ = Left BuildFailureStatus_MiscFailure

    -- Convert success status to wire format
    successStatusToWire :: BuildSuccessStatus -> Word8
    successStatusToWire BuildSuccessStatus_Built = 0
    successStatusToWire BuildSuccessStatus_Substituted = 1
    successStatusToWire BuildSuccessStatus_AlreadyValid = 2
    successStatusToWire BuildSuccessStatus_ResolvesToAlreadyValid = 13

    -- Convert failure status to wire format
    failureStatusToWire :: BuildFailureStatus -> Word8
    failureStatusToWire BuildFailureStatus_PermanentFailure = 3
    failureStatusToWire BuildFailureStatus_InputRejected = 4
    failureStatusToWire BuildFailureStatus_OutputRejected = 5
    failureStatusToWire BuildFailureStatus_TransientFailure = 6
    failureStatusToWire BuildFailureStatus_CachedFailure = 7
    failureStatusToWire BuildFailureStatus_TimedOut = 8
    failureStatusToWire BuildFailureStatus_MiscFailure = 9
    failureStatusToWire BuildFailureStatus_DependencyFailed = 10
    failureStatusToWire BuildFailureStatus_LogLimitExceeded = 11
    failureStatusToWire BuildFailureStatus_NotDeterministic = 12
    failureStatusToWire BuildFailureStatus_NoSubstituters = 14
    failureStatusToWire BuildFailureStatus_HashMismatch = 15

-- *** GCResult

gcResult
  :: StoreDir
  -> NixSerializer ReplySError GCResult
gcResult storeDir = mapErrorS ReplySError_GCResult $ Serializer
  { getS = do
      gcResultDeletedPaths <- getS (hashSet $ storePath storeDir)
      gcResultBytesFreed <- getS int
      Control.Monad.void $ getS (int @Word64) -- obsolete
      pure GCResult{..}
  , putS = \GCResult{..} -> do
      putS (hashSet $ storePath storeDir) gcResultDeletedPaths
      putS int gcResultBytesFreed
      putS (int @Word64) 0 -- obsolete
  }

-- *** GCRoot

gcRoot :: NixSerializer ReplySError GCRoot
gcRoot = Serializer
  { getS = mapGetER $ do
      getS byteString >>= \case
        p | p == censored -> pure GCRoot_Censored
        p -> pure (GCRoot_Path p)
  , putS = putS byteString . \case
      GCRoot_Censored -> censored
      GCRoot_Path p -> p
  }
  where censored = Data.ByteString.Char8.pack "{censored}"

-- *** Missing

missing
  :: StoreDir
  -> NixSerializer ReplySError Missing
missing storeDir = mapErrorS ReplySError_Missing $ Serializer
  { getS = do
      missingWillBuild <- getS (hashSet $ storePath storeDir)
      missingWillSubstitute <- getS (hashSet $ storePath storeDir)
      missingUnknownPaths <- getS (hashSet $ storePath storeDir)
      missingDownloadSize <- getS int
      missingNarSize <- getS int

      pure Missing{..}
  , putS = \Missing{..} -> do
      putS (hashSet $ storePath storeDir) missingWillBuild
      putS (hashSet $ storePath storeDir) missingWillSubstitute
      putS (hashSet $ storePath storeDir) missingUnknownPaths
      putS int missingDownloadSize
      putS int missingNarSize
  }

-- *** Maybe (Metadata StorePath)

maybePathMetadata
  :: StoreDir
  -> NixSerializer ReplySError (Maybe (Metadata StorePath))
maybePathMetadata storeDir = mapErrorS ReplySError_Metadata $ Serializer
  { getS = do
      valid <- getS bool
      if valid
      then pure <$> getS (pathMetadata storeDir)
      else pure Nothing
  , putS = \case
      Nothing -> putS bool False
      Just pm -> putS bool True >> putS (pathMetadata storeDir) pm
  }
