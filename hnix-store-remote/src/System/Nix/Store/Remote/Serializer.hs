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
  , mapReaderS
  , mapErrorS
  -- * Errors
  , SError(..)
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
  , derivation
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
  , derivationOutputTyped
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

import Control.Monad.Except (MonadError, throwError, )
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, withReaderT)
import Control.Monad.Trans.Except (ExceptT, mapExceptT, runExceptT, withExceptT)
import Crypto.Hash (Digest, HashAlgorithm, SHA256)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Fixed (Uni)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Serializer
import Data.Set (Set)
import Data.Some (Some(Some))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Vector (Vector)
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)
import System.Nix.Base (BaseEncoding(Base16, NixBase32))
import System.Nix.Build (BuildMode, BuildResult(..))
import System.Nix.ContentAddress (ContentAddress)
import System.Nix.Derivation (Derivation(..), DerivationOutput(..))
import System.Nix.DerivedPath (DerivedPath(..), ParseOutputsError)
import System.Nix.Hash (HashAlgo(..))
import System.Nix.JSON ()
import System.Nix.OutputName (OutputName)
import System.Nix.Realisation (DerivationOutputError, Realisation(..), RealisationWithId(..))
import System.Nix.Signature (Signature, NarSignature)
import System.Nix.Store.Types (FileIngestionMethod(..), RepairMode(..))
import System.Nix.StorePath (HasStoreDir(..), InvalidNameError, InvalidPathError, StorePath, StorePathHashPart, StorePathName)
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust(..))
import System.Nix.Store.Remote.Types

import qualified Control.Monad
import qualified Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.Attoparsec.Text
import qualified Data.Bifunctor
import qualified Data.Bits
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy
import qualified Data.Coerce
import qualified Data.HashSet
import qualified Data.Map.Strict
import qualified Data.Maybe
import qualified Data.Serialize.Get
import qualified Data.Serialize.Put
import qualified Data.Set
import qualified Data.Some
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Time.Clock.POSIX
import qualified Data.Vector
import qualified System.Nix.Base
import qualified System.Nix.ContentAddress
import qualified System.Nix.DerivedPath
import qualified System.Nix.Hash
import qualified System.Nix.OutputName
import qualified System.Nix.Realisation
import qualified System.Nix.Signature
import qualified System.Nix.StorePath

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

mapErrorST
  :: Functor m
  => (e -> e')
  -> SerialT r e m a
  -> SerialT r e' m a
mapErrorST f =
    SerialT
    . withExceptT f
    . _unSerialT

mapErrorS
  :: (e -> e')
  -> NixSerializer r e a
  -> NixSerializer r e' a
mapErrorS f s = Serializer
  { getS = mapErrorST f $ getS s
  , putS = mapErrorST f . putS s
  }

mapReaderST
  :: Functor m
  => (r' -> r)
  -> SerialT r e m a
  -> SerialT r' e m a
mapReaderST f =
  SerialT
  . (mapExceptT . withReaderT) f
  . _unSerialT

mapReaderS
  :: (r' -> r)
  -> NixSerializer r e a
  -> NixSerializer r' e a
mapReaderS f s = Serializer
  { getS = mapReaderST f $ getS s
  , putS = mapReaderST f . putS s
  }

-- * NixSerializer

type NixSerializer r e = Serializer (SerialT r e)

-- * Errors

data SError
  = SError
  | SError_BadPadding
      { badPaddingStr :: ByteString
      , badPaddingLen :: Int
      , badPaddingPads :: [Word8]
      }
  | SError_ContentAddress String
  | SError_DerivedPath ParseOutputsError
  | SError_DerivationOutput DerivationOutputError
  | SError_Digest String
  | SError_EnumOutOfMinBound Int
  | SError_EnumOutOfMaxBound Int
  | SError_HashAlgo String
  | SError_IllegalBool Word64
  | SError_InvalidNixBase32
  | SError_JSONDecoding String
  | SError_NarHashMustBeSHA256
  | SError_NotYetImplemented String (ForPV ProtoVersion)
  | SError_Name InvalidNameError
  | SError_Path InvalidPathError
  | SError_Signature String
  deriving (Eq, Ord, Generic, Show)

data ForPV a
  = ForPV_Newer a
  | ForPV_Older a
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

bool :: NixSerializer r SError Bool
bool = Serializer
  { getS = getS (int @Word64) >>= \case
      0 -> pure False
      1 -> pure True
      x -> throwError $ SError_IllegalBool x
  , putS = \case
      False -> putS (int @Word8) 0
      True  -> putS (int @Word8) 1
  }

byteString :: NixSerializer r SError ByteString
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
      lift $ Data.Serialize.Put.putByteString x
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
        (lift $ Data.Serialize.Put.putWord8 0)

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
  => NixSerializer r SError a
enum = Serializer
  { getS = getS int >>= toEnumCheckBoundsM
  , putS = putS int . fromEnum
  }

text :: NixSerializer r SError Text
text = mapIsoSerializer
  Data.Text.Encoding.decodeUtf8
  Data.Text.Encoding.encodeUtf8
  byteString

-- TODO Parser Builder
_textBuilder :: NixSerializer r SError Builder
_textBuilder = Serializer
  { getS = Data.Text.Lazy.Builder.fromText <$> getS text
  , putS = putS text . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
  }

maybeText :: NixSerializer r SError (Maybe Text)
maybeText = mapIsoSerializer
  (\case
    t | Data.Text.null t -> Nothing
    t | otherwise -> Just t
  )
  (Data.Maybe.fromMaybe mempty)
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

json
  :: ( FromJSON a
     , ToJSON a
     )
  => NixSerializer r SError a
json =
  mapPrismSerializer
    ( Data.Bifunctor.first SError_JSONDecoding
      . Data.Aeson.eitherDecode
    )
    Data.Aeson.encode
    $ mapIsoSerializer
        Data.ByteString.Lazy.fromStrict
        Data.ByteString.Lazy.toStrict
        byteString

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

storePath :: HasStoreDir r => NixSerializer r SError StorePath
storePath = Serializer
  { getS = do
      sd <- Control.Monad.Reader.asks hasStoreDir
      System.Nix.StorePath.parsePath sd <$> getS byteString
      >>=
        either
          (throwError . SError_Path)
          pure
  , putS = \p -> do
      sd <- Control.Monad.Reader.asks hasStoreDir
      putS
        byteString
        $ System.Nix.StorePath.storePathToRawFilePath sd p
  }

maybePath
  :: HasStoreDir r
  => NixSerializer r SError (Maybe StorePath)
maybePath = Serializer
  { getS = do
      getS maybeText >>= \case
        Nothing -> pure Nothing
        Just t -> do
          sd <- Control.Monad.Reader.asks hasStoreDir
          either
            (throwError . SError_Path)
            (pure . pure)
            $ System.Nix.StorePath.parsePathFromText sd t

  , putS = \case
      Nothing -> putS maybeText Nothing
      Just p -> do
        sd <- Control.Monad.Reader.asks hasStoreDir
        putS text $ System.Nix.StorePath.storePathToText sd p
  }

storePathHashPart :: NixSerializer r SError StorePathHashPart
storePathHashPart =
  mapIsoSerializer
    System.Nix.StorePath.unsafeMakeStorePathHashPart
    System.Nix.StorePath.unStorePathHashPart
    $ mapPrismSerializer
        (Data.Bifunctor.first (pure SError_InvalidNixBase32)
         . System.Nix.Base.decodeWith NixBase32)
        (System.Nix.Base.encodeWith NixBase32)
        text

storePathName :: NixSerializer r SError StorePathName
storePathName =
  mapPrismSerializer
    (Data.Bifunctor.first SError_Name
     . System.Nix.StorePath.mkStorePathName)
    System.Nix.StorePath.unStorePathName
    text

pathMetadata
  :: HasStoreDir r
  => NixSerializer r SError (Metadata StorePath)
pathMetadata = Serializer
  { getS = do
      metadataDeriverPath <- getS maybePath

      digest' <- getS $ digest Base16
      let metadataNarHash = System.Nix.Hash.HashAlgo_SHA256 :=> digest'

      metadataReferences <- getS $ hashSet storePath
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
      putS maybePath metadataDeriverPath

      let putNarHash
            :: DSum HashAlgo Digest
            -> SerialT r SError PutM ()
          putNarHash = \case
            System.Nix.Hash.HashAlgo_SHA256 :=> d
              -> putS (digest @SHA256 Base16) d
            _ -> throwError SError_NarHashMustBeSHA256

      putNarHash metadataNarHash

      putS (hashSet storePath) metadataReferences
      putS time metadataRegistrationTime
      putS int $ Data.Maybe.fromMaybe 0 metadataNarBytes
      putS storePathTrust metadataTrust
      putS (set narSignature) metadataSigs
      putS maybeContentAddress metadataContentAddress
  }
  where
    maybeContentAddress
      :: NixSerializer r SError (Maybe ContentAddress)
    maybeContentAddress =
      mapPrismSerializer
        (maybe
          (pure Nothing)
           $ Data.Bifunctor.bimap
               SError_ContentAddress
               Just
           . System.Nix.ContentAddress.parseContentAddress
        )
        (fmap System.Nix.ContentAddress.buildContentAddress)
        maybeText

    storePathTrust
      :: NixSerializer r SError StorePathTrust
    storePathTrust =
      mapIsoSerializer
        (\case False -> BuiltElsewhere; True -> BuiltLocally)
        (\case BuiltElsewhere -> False; BuiltLocally -> True)
        bool

-- * OutputName

outputName :: NixSerializer r SError OutputName
outputName =
  mapPrismSerializer
    (Data.Bifunctor.first SError_Name
     . System.Nix.OutputName.mkOutputName)
    System.Nix.OutputName.unOutputName
    text

-- * Signatures

signature
  :: NixSerializer r SError Signature
signature =
  mapPrismSerializer
    (Data.Bifunctor.first SError_Signature
     . Data.Attoparsec.Text.parseOnly
         System.Nix.Signature.signatureParser)
    (System.Nix.Signature.signatureToText)
    text

narSignature
  :: NixSerializer r SError NarSignature
narSignature =
  mapPrismSerializer
    (Data.Bifunctor.first SError_Signature
     . Data.Attoparsec.Text.parseOnly
         System.Nix.Signature.narSignatureParser)
    (System.Nix.Signature.narSignatureToText)
    text

-- * Some HashAlgo

someHashAlgo :: NixSerializer r SError (Some HashAlgo)
someHashAlgo =
  mapPrismSerializer
    (Data.Bifunctor.first SError_HashAlgo
     . System.Nix.Hash.textToAlgo)
    (Data.Some.foldSome System.Nix.Hash.algoToText)
    text

-- * Digest

digest
  :: forall a r
   . HashAlgorithm a
  => BaseEncoding
  -> NixSerializer r SError (Digest a)
digest base =
  mapIsoSerializer
    Data.Coerce.coerce
    Data.Coerce.coerce
    $ mapPrismSerializer
        (Data.Bifunctor.first SError_Digest
         . System.Nix.Hash.decodeDigestWith @a base)
        (System.Nix.Hash.encodeDigestWith base)
        $ text

-- * DSum HashAlgo Digest

namedDigest :: NixSerializer r SError (DSum HashAlgo Digest)
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
  :: HasStoreDir r
  => NixSerializer r SError (DerivationOutput StorePath Text)
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
  => NixSerializer r SError (Derivation StorePath Text)
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

-- * DerivedPath

derivedPathNew
  :: HasStoreDir r
  => NixSerializer r SError DerivedPath
derivedPathNew = Serializer
  { getS = do
      root <- Control.Monad.Reader.asks hasStoreDir
      p <- getS text
      case System.Nix.DerivedPath.parseDerivedPath root p of
        Left err -> throwError $ SError_DerivedPath err
        Right x -> pure x
  , putS = \d -> do
      root <- Control.Monad.Reader.asks hasStoreDir
      putS text (System.Nix.DerivedPath.derivedPathToText root d)
  }

derivedPath
  :: ( HasProtoVersion r
     , HasStoreDir r
     )
  => NixSerializer r SError DerivedPath
derivedPath = Serializer
  { getS = do
      pv <- Control.Monad.Reader.asks hasProtoVersion
      if pv < ProtoVersion 1 30
        then DerivedPath_Opaque <$> getS storePath
        else getS derivedPathNew
  , putS = \d -> do
      pv <- Control.Monad.Reader.asks hasProtoVersion
      if pv < ProtoVersion 1 30
        then case d of
          DerivedPath_Opaque p -> putS storePath p
          _ -> throwError
                $ SError_NotYetImplemented
                    "DerivedPath_Built"
                    (ForPV_Older pv)
        else putS derivedPathNew d
  }

-- * Build

buildMode :: NixSerializer r SError BuildMode
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
  => SerialT r SError m a
  -> SerialT r LoggerSError m a
mapPrimE = mapErrorST LoggerSError_Prim

maybeActivity :: NixSerializer r LoggerSError (Maybe Activity)
maybeActivity = Serializer
  { getS = getS (int @Int) >>= \case
      0 -> pure Nothing
      x -> mapPrimE $ toEnumCheckBoundsM (x - 100) >>= pure . Just
  , putS = \case
      Nothing -> putS (int @Int) 0
      Just act -> putS activity act
  }

activity :: NixSerializer r LoggerSError Activity
activity = Serializer
  { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
  , putS = putS int . (+100) . fromEnum
  }

activityID :: NixSerializer r LoggerSError ActivityID
activityID = mapIsoSerializer ActivityID unActivityID int

activityResult :: NixSerializer r LoggerSError ActivityResult
activityResult = Serializer
  { getS = mapPrimE $ getS int >>= toEnumCheckBoundsM . (+(-100))
  , putS = putS int . (+100) . fromEnum
  }

field :: NixSerializer r LoggerSError Field
field = Serializer
  { getS = getS (int @Word8) >>= \case
      0 -> Field_LogInt <$> getS int
      1 -> Field_LogStr <$> mapPrimE (getS text)
      x -> throwError $ LoggerSError_UnknownLogFieldType x
  , putS = \case
      Field_LogInt x -> putS int (0 :: Word8) >> putS int x
      Field_LogStr x -> putS int (1 :: Word8) >> mapPrimE (putS text x)
  }

trace :: NixSerializer r LoggerSError Trace
trace = Serializer
  { getS = do
      tracePosition <- (\case 0 -> Nothing; x -> Just x) <$> getS (int @Int)
      traceHint <- mapPrimE $ getS text
      pure Trace{..}
  , putS = \Trace{..} -> do
      putS int $ Data.Maybe.fromMaybe 0 tracePosition
      mapPrimE $ putS text traceHint
  }

basicError :: NixSerializer r LoggerSError BasicError
basicError = Serializer
  { getS = do
      basicErrorMessage <- mapPrimE $ getS text
      basicErrorExitStatus <- getS int
      pure BasicError{..}

  , putS = \BasicError{..} -> do
      mapPrimE $ putS text basicErrorMessage
      putS int basicErrorExitStatus
  }

errorInfo :: NixSerializer r LoggerSError ErrorInfo
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
        putS int $ Data.Maybe.fromMaybe 0 errorInfoPosition
      putS (list trace) errorInfoTraces
  }

loggerOpCode :: NixSerializer r LoggerSError LoggerOpCode
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
  :: HasProtoVersion r
  => NixSerializer r LoggerSError Logger
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
          mapPrimE $ putS text s

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
            Left _ | minor >= 26 -> throwError $ LoggerSError_TooNewForBasicError
            Left e | otherwise -> putS basicError e
            Right _ | minor < 26 -> throwError $ LoggerSError_TooOldForErrorInfo
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

verbosity :: NixSerializer r LoggerSError Verbosity
verbosity = Serializer
  { getS = mapPrimE $ getS enum
  , putS = mapPrimE . putS enum
  }

-- * Handshake

data HandshakeSError
  = HandshakeSError_InvalidWorkerMagic Word64
  | HandshakeSError_InvalidTrustedFlag Word8
  deriving (Eq, Ord, Generic, Show)

workerMagic :: NixSerializer r HandshakeSError WorkerMagic
workerMagic = Serializer
  { getS = do
      c <- getS int
      either
        (pure $ throwError (HandshakeSError_InvalidWorkerMagic c))
        pure
        $ word64ToWorkerMagic c
  , putS = putS int . workerMagicToWord64
  }

trustedFlag :: NixSerializer r HandshakeSError (Maybe TrustedFlag)
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

storeText :: NixSerializer r SError StoreText
storeText = Serializer
  { getS = do
      storeTextName <- getS storePathName
      storeTextText <- getS text
      pure StoreText{..}
  , putS = \StoreText{..} -> do
      putS storePathName storeTextName
      putS text storeTextText
  }

workerOp :: NixSerializer r SError WorkerOp
workerOp = enum

-- * Request

data RequestSError
  = RequestSError_NotYetImplemented WorkerOp
  | RequestSError_ReservedOp WorkerOp
  | RequestSError_PrimGet SError
  | RequestSError_PrimPut SError
  | RequestSError_PrimWorkerOp SError
  deriving (Eq, Ord, Generic, Show)

storeRequest
  :: ( HasProtoVersion r
     , HasStoreDir r
     )
  => NixSerializer r RequestSError (Some StoreRequest)
storeRequest = Serializer
  { getS = mapErrorST RequestSError_PrimWorkerOp (getS workerOp) >>= \case
      WorkerOp_AddToStore -> mapGetE $ do
        pathName <- getS storePathName
        _fixed <- getS bool -- obsolete
        recursive <- getS enum
        hashAlgo <- getS someHashAlgo

        -- not supported by ProtoVersion < 1.25
        let repair = RepairMode_DontRepair

        pure $ Some (AddToStore pathName recursive hashAlgo repair)

      WorkerOp_AddToStoreNar -> mapGetE $ do
        storePath' <- getS storePath
        metadata <- getS pathMetadata
        repair <- getS bool
        let repairMode = if repair then RepairMode_DoRepair else RepairMode_DontRepair
        dontCheckSigs <- getS bool
        let checkSigs = if dontCheckSigs then CheckMode_DontCheck else CheckMode_DoCheck

        pure $ Some (AddToStoreNar storePath' metadata repairMode checkSigs)

      WorkerOp_AddTextToStore -> mapGetE $ do
        txt <- getS storeText
        paths <- getS (hashSet storePath)
        let repair = RepairMode_DontRepair
        pure $ Some (AddTextToStore txt paths repair)

      WorkerOp_AddSignatures -> mapGetE $ do
        path <- getS storePath
        signatures <- getS (set signature)
        pure $ Some (AddSignatures path signatures)

      WorkerOp_AddIndirectRoot -> mapGetE $ do
        Some . AddIndirectRoot <$> getS storePath

      WorkerOp_AddTempRoot -> mapGetE $ do
        Some . AddTempRoot <$> getS storePath

      WorkerOp_BuildPaths -> mapGetE $ do
        derived <- getS (set derivedPath)
        buildMode' <- getS buildMode
        pure $ Some (BuildPaths derived buildMode')

      WorkerOp_BuildDerivation -> mapGetE $ do
        path <- getS storePath
        drv <- getS derivation
        buildMode' <- getS buildMode
        pure $ Some (BuildDerivation path drv buildMode')

      WorkerOp_CollectGarbage -> mapGetE $ do
        gcOptionsOperation <- getS enum
        gcOptionsPathsToDelete <- getS (hashSet storePath)
        gcOptionsIgnoreLiveness <- getS bool
        gcOptionsMaxFreed <- getS int
        -- obsolete fields
        Control.Monad.forM_ [0..(2 :: Word8)]
          $ pure $ getS (int @Word8)
        pure $ Some (CollectGarbage GCOptions{..})

      WorkerOp_EnsurePath -> mapGetE $ do
        Some . EnsurePath <$> getS storePath

      WorkerOp_FindRoots -> mapGetE $ do
        pure $ Some FindRoots

      WorkerOp_IsValidPath -> mapGetE $ do
        Some . IsValidPath <$> getS storePath

      WorkerOp_NarFromPath -> mapGetE $ do
        Some . NarFromPath <$> getS storePath

      WorkerOp_QueryValidPaths -> mapGetE $ do
        paths <- getS (hashSet storePath)
        substituteMode <- getS enum
        pure $ Some (QueryValidPaths paths substituteMode)

      WorkerOp_QueryAllValidPaths -> mapGetE $ do
        pure $ Some QueryAllValidPaths

      WorkerOp_QuerySubstitutablePaths -> mapGetE $ do
        Some . QuerySubstitutablePaths <$> getS (hashSet storePath)

      WorkerOp_QueryPathInfo -> mapGetE $ do
        Some . QueryPathInfo <$> getS storePath

      WorkerOp_QueryReferrers -> mapGetE $ do
        Some . QueryReferrers <$> getS storePath

      WorkerOp_QueryValidDerivers -> mapGetE $ do
        Some . QueryValidDerivers <$> getS storePath

      WorkerOp_QueryDerivationOutputs -> mapGetE $ do
        Some . QueryDerivationOutputs <$> getS storePath

      WorkerOp_QueryDerivationOutputNames -> mapGetE $ do
        Some . QueryDerivationOutputNames <$> getS storePath

      WorkerOp_QueryPathFromHashPart -> mapGetE $ do
        Some . QueryPathFromHashPart <$> getS storePathHashPart

      WorkerOp_QueryMissing -> mapGetE $ do
        Some . QueryMissing <$> getS (set derivedPath)

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
      Some (AddToStore pathName recursive hashAlgo _repair) -> mapPutE $ do
        putS workerOp WorkerOp_AddToStore

        putS storePathName pathName
        -- obsolete fixed
        putS bool
          $ not
          $ hashAlgo == Some HashAlgo_SHA256
            && (recursive == FileIngestionMethod_NixArchive)

        putS bool (recursive == FileIngestionMethod_NixArchive)
        putS someHashAlgo hashAlgo

      Some (AddToStoreNar storePath' metadata repair checkSigs) -> mapPutE $ do
        putS workerOp WorkerOp_AddToStoreNar

        putS storePath storePath'
        putS pathMetadata metadata
        putS bool $ repair == RepairMode_DoRepair
        putS bool $ checkSigs == CheckMode_DontCheck

      Some (AddTextToStore txt paths _repair) -> mapPutE $ do
        putS workerOp WorkerOp_AddTextToStore

        putS storeText txt
        putS (hashSet storePath) paths

      Some (AddSignatures path signatures) -> mapPutE $ do
        putS workerOp WorkerOp_AddSignatures

        putS storePath path
        putS (set signature) signatures

      Some (AddIndirectRoot path) -> mapPutE $ do
        putS workerOp WorkerOp_AddIndirectRoot
        putS storePath path

      Some (AddTempRoot path) -> mapPutE $ do
        putS workerOp WorkerOp_AddTempRoot
        putS storePath path

      Some (BuildPaths derived buildMode') -> mapPutE $ do
        putS workerOp WorkerOp_BuildPaths

        putS (set derivedPath) derived
        putS buildMode buildMode'

      Some (BuildDerivation path drv buildMode') -> mapPutE $ do
        putS workerOp WorkerOp_BuildDerivation

        putS storePath path
        putS derivation drv
        putS buildMode buildMode'

      Some (CollectGarbage GCOptions{..}) -> mapPutE $ do
        putS workerOp WorkerOp_CollectGarbage

        putS enum gcOptionsOperation
        putS (hashSet storePath) gcOptionsPathsToDelete
        putS bool gcOptionsIgnoreLiveness
        putS int gcOptionsMaxFreed
        -- obsolete fields
        Control.Monad.forM_ [0..(2 :: Word8)]
          $ pure $ putS int (0 :: Word8)

      Some (EnsurePath path) -> mapPutE $ do
        putS workerOp WorkerOp_EnsurePath
        putS storePath path

      Some FindRoots -> mapPutE $ do
        putS workerOp WorkerOp_FindRoots

      Some (IsValidPath path) -> mapPutE $ do
        putS workerOp WorkerOp_IsValidPath
        putS storePath path

      Some (NarFromPath path) -> mapPutE $ do
        putS workerOp WorkerOp_NarFromPath
        putS storePath path

      Some (QueryValidPaths paths substituteMode) -> mapPutE $ do
        putS workerOp WorkerOp_QueryValidPaths

        putS (hashSet storePath) paths
        putS enum substituteMode

      Some QueryAllValidPaths -> mapPutE $ do
        putS workerOp WorkerOp_QueryAllValidPaths

      Some (QuerySubstitutablePaths paths) -> mapPutE $ do
        putS workerOp WorkerOp_QuerySubstitutablePaths
        putS (hashSet storePath) paths

      Some (QueryPathInfo path) -> mapPutE $ do
        putS workerOp WorkerOp_QueryPathInfo
        putS storePath path

      Some (QueryReferrers path) -> mapPutE $ do
        putS workerOp WorkerOp_QueryReferrers
        putS storePath path

      Some (QueryValidDerivers path) -> mapPutE $ do
        putS workerOp WorkerOp_QueryValidDerivers
        putS storePath path

      Some (QueryDerivationOutputs path) -> mapPutE $ do
        putS workerOp WorkerOp_QueryDerivationOutputs
        putS storePath path

      Some (QueryDerivationOutputNames path) -> mapPutE $ do
        putS workerOp WorkerOp_QueryDerivationOutputNames
        putS storePath path

      Some (QueryPathFromHashPart pathHashPart) -> mapPutE $ do
        putS workerOp WorkerOp_QueryPathFromHashPart
        putS storePathHashPart pathHashPart

      Some (QueryMissing derived) -> mapPutE $ do
        putS workerOp WorkerOp_QueryMissing
        putS (set derivedPath) derived

      Some OptimiseStore -> mapPutE $ do
        putS workerOp WorkerOp_OptimiseStore

      Some SyncWithGC -> mapPutE $ do
        putS workerOp WorkerOp_SyncWithGC

      Some (VerifyStore checkMode repairMode) -> mapPutE $ do
        putS workerOp WorkerOp_VerifyStore
        putS enum checkMode
        putS enum repairMode
  }
  where
    mapGetE
      :: Functor m
      => SerialT r SError m a
      -> SerialT r RequestSError m a
    mapGetE = mapErrorST RequestSError_PrimGet

    mapPutE
      :: Functor m
      => SerialT r SError m a
      -> SerialT r RequestSError m a
    mapPutE = mapErrorST RequestSError_PrimPut

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
  | ReplySError_PrimPut SError
  | ReplySError_DerivationOutput SError
  | ReplySError_GCResult SError
  | ReplySError_Metadata SError
  | ReplySError_Missing SError
  | ReplySError_Realisation SError
  | ReplySError_RealisationWithId SError
  | ReplySError_UnexpectedFalseOpSuccess
  deriving (Eq, Ord, Generic, Show)

mapGetER
  :: Functor m
  => SerialT r SError m a
  -> SerialT r ReplySError m a
mapGetER = mapErrorST ReplySError_PrimGet

mapPutER
  :: Functor m
  => SerialT r SError m a
  -> SerialT r ReplySError m a
mapPutER = mapErrorST ReplySError_PrimPut

-- | Parse a bool returned at the end of simple operations.
-- This is always 1 (@True@) so we assert that it really is so.
-- Errors for these operations are indicated via @Logger_Error@.
opSuccess :: NixSerializer r ReplySError SuccessCodeReply
opSuccess = Serializer
  { getS = do
      retCode <- mapGetER $ getS bool
      Control.Monad.unless
        (retCode == True)
        $ throwError ReplySError_UnexpectedFalseOpSuccess
      pure SuccessCodeReply
  , putS = \_ -> mapPutER $ putS bool True
  }

noop :: a -> NixSerializer r ReplySError a
noop ret = Serializer
  { getS = pure ret
  , putS = \_ -> pure ()
  }

-- *** Realisation

derivationOutputTyped :: NixSerializer r ReplySError (System.Nix.Realisation.DerivationOutput OutputName)
derivationOutputTyped = mapErrorS ReplySError_DerivationOutput $
  mapPrismSerializer
    ( Data.Bifunctor.first SError_DerivationOutput
      . System.Nix.Realisation.derivationOutputParser
          System.Nix.OutputName.mkOutputName
    )
    ( Data.Text.Lazy.toStrict
      . Data.Text.Lazy.Builder.toLazyText
      . System.Nix.Realisation.derivationOutputBuilder
          System.Nix.OutputName.unOutputName
    )
    text

realisation :: NixSerializer r ReplySError Realisation
realisation = mapErrorS ReplySError_Realisation json

realisationWithId :: NixSerializer r ReplySError RealisationWithId
realisationWithId = mapErrorS ReplySError_RealisationWithId json

-- *** BuildResult

buildResult
  :: ( HasProtoVersion r
     , HasStoreDir r
     )
  => NixSerializer r ReplySError BuildResult
buildResult = Serializer
  { getS = do
      pv <- Control.Monad.Reader.asks hasProtoVersion

      buildResultStatus <- mapGetER $ getS enum
      buildResultErrorMessage <- mapGetER $ getS maybeText

      ( buildResultTimesBuilt
        , buildResultIsNonDeterministic
        , buildResultStartTime
        , buildResultStopTime
        ) <-
        if protoVersion_minor pv >= 29
        then mapGetER $ do
          tb <- (\case 0 -> Nothing; x -> Just x) <$> getS int
          nondet <- getS bool
          start <- (\case x | x == t0 -> Nothing; x -> Just x) <$> getS time
          end <- (\case x | x == t0 -> Nothing; x -> Just x) <$> getS time
          pure $ (tb, pure nondet, start, end)
        else pure $ (Nothing, Nothing, Nothing, Nothing)

      buildResultBuiltOutputs <-
        if protoVersion_minor pv >= 28
        then
            pure
          . Data.Map.Strict.fromList
          . map (\(_, RealisationWithId (a, b)) -> (a, b))
          . Data.Map.Strict.toList
          <$> getS (mapS derivationOutputTyped realisationWithId)
        else pure Nothing
      pure BuildResult{..}

  , putS = \BuildResult{..} -> do
      pv <- Control.Monad.Reader.asks hasProtoVersion

      mapPutER $ putS enum buildResultStatus
      mapPutER $ putS maybeText buildResultErrorMessage
      Control.Monad.when (protoVersion_minor pv >= 29) $ mapPutER $ do
        putS int $ Data.Maybe.fromMaybe 0 buildResultTimesBuilt
        putS bool $ Data.Maybe.fromMaybe False buildResultIsNonDeterministic
        putS time $ Data.Maybe.fromMaybe t0 buildResultStartTime
        putS time $ Data.Maybe.fromMaybe t0 buildResultStopTime
      Control.Monad.when (protoVersion_minor pv >= 28)
        $ putS (mapS derivationOutputTyped realisationWithId)
        $ Data.Map.Strict.fromList
        $ map (\(a, b) -> (a, RealisationWithId (a, b)))
        $ Data.Map.Strict.toList
        $ Data.Maybe.fromMaybe mempty buildResultBuiltOutputs
  }
  where
    t0 :: UTCTime
    t0 = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0

-- *** GCResult

gcResult
  :: HasStoreDir r
  => NixSerializer r ReplySError GCResult
gcResult = mapErrorS ReplySError_GCResult $ Serializer
  { getS = do
      gcResultDeletedPaths <- getS (hashSet storePath)
      gcResultBytesFreed <- getS int
      Control.Monad.void $ getS (int @Word64) -- obsolete
      pure GCResult{..}
  , putS = \GCResult{..} -> do
      putS (hashSet storePath) gcResultDeletedPaths
      putS int gcResultBytesFreed
      putS (int @Word64) 0 -- obsolete
  }

-- *** GCRoot

gcRoot :: NixSerializer r ReplySError GCRoot
gcRoot = Serializer
  { getS = mapGetER $ do
      getS byteString >>= \case
        p | p == censored -> pure GCRoot_Censored
        p -> pure (GCRoot_Path p)
  , putS = mapPutER . putS byteString . \case
      GCRoot_Censored -> censored
      GCRoot_Path p -> p
  }
  where censored = Data.ByteString.Char8.pack "{censored}"

-- *** Missing

missing
  :: HasStoreDir r
  => NixSerializer r ReplySError Missing
missing = mapErrorS ReplySError_Missing $ Serializer
  { getS = do
      missingWillBuild <- getS (hashSet storePath)
      missingWillSubstitute <- getS (hashSet storePath)
      missingUnknownPaths <- getS (hashSet storePath)
      missingDownloadSize <- getS int
      missingNarSize <- getS int

      pure Missing{..}
  , putS = \Missing{..} -> do
      putS (hashSet storePath) missingWillBuild
      putS (hashSet storePath) missingWillSubstitute
      putS (hashSet storePath) missingUnknownPaths
      putS int missingDownloadSize
      putS int missingNarSize
  }

-- *** Maybe (Metadata StorePath)

maybePathMetadata
  :: HasStoreDir r
  => NixSerializer r ReplySError (Maybe (Metadata StorePath))
maybePathMetadata = mapErrorS ReplySError_Metadata $ Serializer
  { getS = do
      valid <- getS bool
      if valid
      then pure <$> getS pathMetadata
      else pure Nothing
  , putS = \case
      Nothing -> putS bool False
      Just pm -> putS bool True >> putS pathMetadata pm
  }
