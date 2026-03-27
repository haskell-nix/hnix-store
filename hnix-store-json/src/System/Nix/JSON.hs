{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description : JSON serialization

This module is mostly a stub for now
providing (From|To)JSON for Realisation type
which is required for `-remote`.
-}
module System.Nix.JSON
  ( HashJSON(..)
  ) where

import Control.Applicative ((<|>))
import Crypto.Hash (Digest)
import Data.Aeson
import Data.Aeson.Key qualified
import Data.Aeson.KeyMap qualified
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified
import Data.Attoparsec.Text qualified
import Data.Dependent.Sum
import Data.Foldable (toList)
import Data.Map.Monoidal qualified
import Data.Map.Strict qualified
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified
import Data.Some
import Data.Text (Text)
import Data.Text qualified
import Data.These
import Data.These.Combinators
import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Nix.Base qualified
import System.Nix.Build (BuildResult(..), BuildSuccess(..), BuildFailure(..), BuildSuccessStatus(..), BuildFailureStatus(..))
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.Hash
import System.Nix.OutputName (OutputName, mkOutputName)
import System.Nix.OutputName qualified
import System.Nix.Realisation (BuildTraceKey(..), Realisation(..), RealisationWithId(..))
import System.Nix.Signature (Signature, NamedSignature(..))
import System.Nix.Signature qualified
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart, mkStorePathName, unStorePathName, parseBasePathFromText)
import System.Nix.StorePath qualified
import System.Nix.StorePath.Metadata (Metadata(..), StorePathTrust(..))

instance ToJSON StorePathName where
  toJSON = toJSON . System.Nix.StorePath.unStorePathName
  toEncoding = toEncoding . System.Nix.StorePath.unStorePathName

instance FromJSON StorePathName where
  parseJSON =
    withText "StorePathName"
    ( either (fail . show) pure
    . System.Nix.StorePath.mkStorePathName)

instance ToJSON StorePathHashPart where
  toJSON = toJSON . System.Nix.StorePath.storePathHashPartToText
  toEncoding = toEncoding . System.Nix.StorePath.storePathHashPartToText

instance FromJSON StorePathHashPart where
  parseJSON =
    withText "StorePathHashPart"
    ( either
        (fail . show)
        (pure . System.Nix.StorePath.unsafeMakeStorePathHashPart)
    . System.Nix.Base.decodeWith NixBase32
    )

instance ToJSON StorePath where
  toJSON = toJSON . System.Nix.StorePath.storePathBaseToText
  toEncoding = toEncoding . System.Nix.StorePath.storePathBaseToText

instance FromJSON StorePath where
  parseJSON =
    withText "StorePath"
    ( either
        (fail . show)
        pure
    . parseBasePathFromText
    )

instance ToJSONKey StorePath where
  toJSONKey = Data.Aeson.Types.toJSONKeyText System.Nix.StorePath.storePathBaseToText

instance FromJSONKey StorePath where
  fromJSONKey = FromJSONKeyTextParser $
    either (fail . show) pure . parseBasePathFromText

deriving newtype instance FromJSON DerivedPathMap
deriving newtype instance ToJSON DerivedPathMap

instance FromJSON ChildNode where
  parseJSON = withObject "ChildNode" $ \obj -> do
    outputs <- obj .: "outputs"
    dynamicOutputs <- obj .: "dynamicOutputs"
    ChildNode <$> case (Data.Set.null outputs, Data.Map.Monoidal.null dynamicOutputs) of
      (False, True) -> pure $ This outputs
      (True, False) -> pure $ That dynamicOutputs
      (False, False) -> pure $ These outputs dynamicOutputs
      (True, True) -> fail "outputs and dynamic outputs cannot both be empty"

instance ToJSON ChildNode where
  toJSON (ChildNode cn) = object
    [ "outputs" .= fromMaybe Data.Set.empty (justHere cn)
    , "dynamicOutputs" .= fromMaybe Data.Map.Monoidal.empty (justThere cn)
    ]

-- | Input-addressed derivation output JSON instance
instance FromJSON InputAddressedDerivationOutput where
  parseJSON = withObject "InputAddressedDerivationOutput" $ \obj ->
    InputAddressedDerivationOutput <$> obj .: "path"

instance ToJSON InputAddressedDerivationOutput where
  toJSON (InputAddressedDerivationOutput path) =
    object ["path" .= path]

-- | Fixed derivation output JSON instance
instance FromJSON FixedDerivationOutput where
  parseJSON = withObject "FixedDerivationOutput" $ \obj -> do
    method <- obj .: "method" >>= either fail pure . textToMethod
    HashJSON hash <- obj .: "hash"
    pure $ FixedDerivationOutput method hash

instance ToJSON FixedDerivationOutput where
  toJSON (FixedDerivationOutput method hash) =
    object
      [ "method" .= methodToText method
      , "hash" .= HashJSON hash
      ]

-- | Content-addressed derivation output JSON instance
instance FromJSON ContentAddressedDerivationOutput where
  parseJSON = withObject "ContentAddressedDerivationOutput" $ \obj -> do
    method <- obj .: "method" >>= either fail pure . textToMethod
    hashAlgo <- obj .: "hashAlgo" >>= either fail pure . textToAlgo
    pure $ ContentAddressedDerivationOutput method hashAlgo

instance ToJSON ContentAddressedDerivationOutput where
  toJSON (ContentAddressedDerivationOutput method (Some hashAlgo)) =
    object
      [ "method" .= methodToText method
      , "hashAlgo" .= algoToText hashAlgo
      ]

-- Helper to parse DerivationOutputs from JSON
parseDerivationOutputs :: Object -> Parser DerivationOutputs
parseDerivationOutputs obj = do
  let outputPairs = Data.Aeson.KeyMap.toList obj
  case outputPairs of
    [] -> pure $ DerivationType_InputAddressing :=> mempty
    (_, firstVal) : _ -> do
      -- Parse the first output to determine the derivation type
      withObject "first output" (\first -> do
        -- Determine type by checking which fields are present
        let hasPath = Data.Aeson.KeyMap.member "path" first
            hasMethod = Data.Aeson.KeyMap.member "method" first
            hasHash = Data.Aeson.KeyMap.member "hash" first
            hasHashAlgo = Data.Aeson.KeyMap.member "hashAlgo" first

        case (hasPath, hasMethod, hasHash, hasHashAlgo) of
          (True, False, False, False) -> do
            -- Input-addressed
            outputs <- Data.Map.Strict.fromList <$> mapM (\(k, v) -> do
              outputName <- either (fail . show) pure $ mkOutputName $ Data.Aeson.Key.toText k
              output <- parseJSON v
              pure (outputName, output)) outputPairs
            pure $ DerivationType_InputAddressing :=> outputs
          (False, True, True, False) -> do
            -- Fixed
            outputs <- Data.Map.Strict.fromList <$> mapM (\(k, v) -> do
              outputName <- either (fail . show) pure $ mkOutputName $ Data.Aeson.Key.toText k
              output <- parseJSON v
              pure (outputName, output)) outputPairs
            pure $ DerivationType_Fixed :=> outputs
          (False, True, False, True) -> do
            -- Content-addressed
            outputs <- Data.Map.Strict.fromList <$> mapM (\(k, v) -> do
              outputName <- either (fail . show) pure $ mkOutputName $ Data.Aeson.Key.toText k
              output <- parseJSON v
              pure (outputName, output)) outputPairs
            pure $ DerivationType_ContentAddressing :=> outputs
          _ -> fail $ "Invalid output format: " <> show (hasPath, hasMethod, hasHash, hasHashAlgo)
        ) firstVal

-- Helper to serialize DerivationOutputs to JSON
derivationOutputsToJSON :: DerivationOutputs -> Value
derivationOutputsToJSON (ty :=> outputs) = case ty of
  DerivationType_InputAddressing -> toJSON outputs
  DerivationType_Fixed -> toJSON outputs
  DerivationType_ContentAddressing -> toJSON outputs

instance FromJSONKey StorePathName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . show) pure . mkStorePathName

instance ToJSONKey StorePathName where
  toJSONKey = Data.Aeson.Types.toJSONKeyText unStorePathName

deriving newtype instance FromJSON OutputName
deriving newtype instance ToJSON OutputName
deriving newtype instance FromJSONKey OutputName
deriving newtype instance ToJSONKey OutputName

-- | TODO: hacky, we need to stop assuming StoreDir for
-- StorePath to and from JSON
instance FromJSON Derivation where
  parseJSON = withObject "Derivation" $ \v -> do
    name <- v .: "name"
    inputs <- v .: "inputs" >>= \inputsObj -> DerivationInputs
      <$> inputsObj .: "srcs"
      <*> inputsObj .: "drvs"
    outputs <- v .: "outputs" >>= parseDerivationOutputs
    Derivation name outputs
      <$> pure inputs
      <*> v .: "system"
      <*> v .: "builder"
      <*> v .: "args"
      <*> v .: "env"

instance ToJSON Derivation where
  toJSON (Derivation name outputs (DerivationInputs inputSrcs inputDrvs) system builder args env) =
    object [ "name" .= name
           , "outputs" .= derivationOutputsToJSON outputs
           , "inputs" .= object
              [ "srcs" .= inputSrcs
              , "drvs" .= inputDrvs
              ]
           , "system" .= system
           , "builder" .= builder
           , "args" .= args
           , "env" .= env
           ]

instance ToJSON BuildTraceKey where
  toJSON btk =
    object
      [ "drvPath" .= buildTraceKeyDrvPath btk
      , "outputName" .= buildTraceKeyOutput btk
      ]

instance FromJSON BuildTraceKey where
  parseJSON = withObject "BuildTraceKey" $ \o ->
    BuildTraceKey
      <$> o .: "drvPath"
      <*> o .: "outputName"

instance ToJSON Signature where
  toJSON = toJSON . System.Nix.Signature.signatureToText
  toEncoding = toEncoding . System.Nix.Signature.signatureToText

instance FromJSON Signature where
  parseJSON =
    withText "Signature"
    ( either
        (fail . show)
        pure
    . Data.Attoparsec.Text.parseOnly
        System.Nix.Signature.signatureParser
    )

instance ToJSON NamedSignature where
  toJSON ns =
    object
      [ "keyName" .= publicKey ns
      , "sig" .= sig ns
      ]

instance FromJSON NamedSignature where
  parseJSON = withObject "NamedSignature" $ \o -> do
    keyName <- o .: "keyName"
    sigVal <- o .: "sig"
    pure $ NamedSignature { publicKey = keyName, sig = sigVal }

-- | Needed to avoid overlapping instances
newtype HashJSON = HashJSON { unHashJSON :: DSum HashAlgo Digest }
  deriving (Eq, Show)

instance ToJSON HashJSON where
  toJSON (HashJSON (algo :=> digest)) =
    -- SRI format: "algo-base64hash"
    toJSON $ algoToText algo <> "-" <> encodeDigestWith Base64 digest

instance FromJSON HashJSON where
  parseJSON = withText "HashJSON" $ \t -> do
    -- SRI format: "algo-base64hash"
    let (algoText, rest) = Data.Text.breakOn "-" t
    case Data.Text.uncons rest of
      Nothing -> fail "HashJSON: missing '-' separator"
      Just ('-', _hashText) ->
        either fail (pure . HashJSON) $ mkNamedDigest algoText t
      _ -> fail "HashJSON: missing '-' separator"

instance ToJSON ContentAddress where
  toJSON (ContentAddress method digest) =
    object
      [ "hash" .= HashJSON digest
      , "method" .= methodToText method
      ]

instance FromJSON ContentAddress where
  parseJSON = withObject "ContentAddress" $ \obj -> do
    HashJSON digest <- obj .: "hash"
    methodText <- obj .: "method"
    method <- either fail pure $ textToMethod methodText
    pure $ ContentAddress method digest

-- | OutputsSpec encodes as an array. The wildcard "*" is a singleton
-- array ["*"] to match upstream Nix JSON format.
instance ToJSON OutputsSpec where
  toJSON OutputsSpec_All = toJSON ["*" :: Text]
  toJSON (OutputsSpec_Names names) = toJSON $ Data.Set.toList names

instance FromJSON OutputsSpec where
  parseJSON = withArray "OutputsSpec" $ \arr -> do
    outputs <- mapM parseJSON (toList arr)
    if outputs == ["*" :: Text]
      then pure OutputsSpec_All
      else do
        names <- mapM (either (fail . show) pure . System.Nix.OutputName.mkOutputName) outputs
        pure $ OutputsSpec_Names (Data.Set.fromList names)

instance ToJSON SingleDerivedPath where
  toJSON (SingleDerivedPath_Opaque path) = toJSON path
  toJSON (SingleDerivedPath_Built drvPath output) =
    object
      [ "drvPath" .= toJSON drvPath
      , "output" .= output
      ]

instance FromJSON SingleDerivedPath where
  parseJSON v = parseOpaque v <|> parseBuilt v
    where
      parseOpaque = fmap SingleDerivedPath_Opaque . parseJSON
      parseBuilt = withObject "SingleDerivedPath_Built" $ \obj ->
        SingleDerivedPath_Built
          <$> obj .: "drvPath"
          <*> obj .: "output"

instance ToJSON DerivedPath where
  toJSON (DerivedPath_Opaque path) = toJSON path
  toJSON (DerivedPath_Built drvPath outputs) =
    object
      [ "drvPath" .= toJSON drvPath
      , "outputs" .= outputs
      ]

instance FromJSON DerivedPath where
  parseJSON v = parseOpaque v <|> parseBuilt v
    where
      parseOpaque = fmap DerivedPath_Opaque . parseJSON
      parseBuilt = withObject "DerivedPath_Built" $ \obj ->
        DerivedPath_Built
          <$> obj .: "drvPath"
          <*> obj .: "outputs"

instance ToJSON Realisation where
  toJSON r =
    object
      [ "outPath" .= realisationOutPath r
      , "signatures" .= realisationSignatures r
      ]

instance FromJSON Realisation where
  parseJSON = withObject "Realisation" $ \o ->
    Realisation
      <$> o .: "outPath"
      <*> o .:? "signatures" .!= mempty

-- For a keyed version of Realisation
-- we use RealisationWithId (BuildTraceKey, Realisation)
-- instead of Realisation.id :: BuildTraceKey
-- field.
instance ToJSON RealisationWithId where
  toJSON (RealisationWithId (drvOut, r)) =
    object
      [ "key" .= drvOut
      , "value" .= r
      ]

instance FromJSON RealisationWithId where
  parseJSON = withObject "RealisationWithId" $ \o -> do
    drvOut <- o .: "key"
    r <- o .: "value"
    pure (RealisationWithId (drvOut, r))

-- BuildSuccessStatus enum to/from JSON as strings
instance ToJSON BuildSuccessStatus where
  toJSON BuildSuccessStatus_Built = String "Built"
  toJSON BuildSuccessStatus_Substituted = String "Substituted"
  toJSON BuildSuccessStatus_AlreadyValid = String "AlreadyValid"
  toJSON BuildSuccessStatus_ResolvesToAlreadyValid = String "ResolvesToAlreadyValid"

instance FromJSON BuildSuccessStatus where
  parseJSON = withText "BuildSuccessStatus" $ \case
    "Built" -> pure BuildSuccessStatus_Built
    "Substituted" -> pure BuildSuccessStatus_Substituted
    "AlreadyValid" -> pure BuildSuccessStatus_AlreadyValid
    "ResolvesToAlreadyValid" -> pure BuildSuccessStatus_ResolvesToAlreadyValid
    other -> fail $ "Unknown BuildSuccessStatus: " ++ Data.Text.unpack other

-- BuildFailureStatus enum to/from JSON as strings
instance ToJSON BuildFailureStatus where
  toJSON BuildFailureStatus_PermanentFailure = String "PermanentFailure"
  toJSON BuildFailureStatus_InputRejected = String "InputRejected"
  toJSON BuildFailureStatus_OutputRejected = String "OutputRejected"
  toJSON BuildFailureStatus_TransientFailure = String "TransientFailure"
  toJSON BuildFailureStatus_CachedFailure = String "CachedFailure"
  toJSON BuildFailureStatus_TimedOut = String "TimedOut"
  toJSON BuildFailureStatus_MiscFailure = String "MiscFailure"
  toJSON BuildFailureStatus_DependencyFailed = String "DependencyFailed"
  toJSON BuildFailureStatus_LogLimitExceeded = String "LogLimitExceeded"
  toJSON BuildFailureStatus_NotDeterministic = String "NotDeterministic"
  toJSON BuildFailureStatus_NoSubstituters = String "NoSubstituters"
  toJSON BuildFailureStatus_HashMismatch = String "HashMismatch"

instance FromJSON BuildFailureStatus where
  parseJSON = withText "BuildFailureStatus" $ \case
    "PermanentFailure" -> pure BuildFailureStatus_PermanentFailure
    "InputRejected" -> pure BuildFailureStatus_InputRejected
    "OutputRejected" -> pure BuildFailureStatus_OutputRejected
    "TransientFailure" -> pure BuildFailureStatus_TransientFailure
    "CachedFailure" -> pure BuildFailureStatus_CachedFailure
    "TimedOut" -> pure BuildFailureStatus_TimedOut
    "MiscFailure" -> pure BuildFailureStatus_MiscFailure
    "DependencyFailed" -> pure BuildFailureStatus_DependencyFailed
    "LogLimitExceeded" -> pure BuildFailureStatus_LogLimitExceeded
    "NotDeterministic" -> pure BuildFailureStatus_NotDeterministic
    "NoSubstituters" -> pure BuildFailureStatus_NoSubstituters
    "HashMismatch" -> pure BuildFailureStatus_HashMismatch
    other -> fail $ "Unknown BuildFailureStatus: " ++ Data.Text.unpack other

-- BuildResult JSON instances
instance ToJSON BuildResult where
  toJSON (BuildResult status timesBuilt startTime stopTime cpuUser cpuSystem) =
    case status of
      Right (BuildSuccess successStatus builtOutputs) ->
        object $ concat
          [ [ "success" .= True
            , "status" .= successStatus
            , "builtOutputs" .= builtOutputs
            , "timesBuilt" .= timesBuilt
            , "startTime" .= (floor (realToFrac (diffUTCTime startTime (posixSecondsToUTCTime 0)) :: Double) :: Integer)
            , "stopTime" .= (floor (realToFrac (diffUTCTime stopTime (posixSecondsToUTCTime 0)) :: Double) :: Integer)
            ]
          , maybeToList $ ("cpuUser" .=) <$> cpuUser
          , maybeToList $ ("cpuSystem" .=) <$> cpuSystem
          ]
      Left (BuildFailure failureStatus errorMsg isNonDeterministic) ->
        object
          [ "success" .= False
          , "status" .= failureStatus
          , "errorMsg" .= errorMsg
          , "isNonDeterministic" .= isNonDeterministic
          , "timesBuilt" .= timesBuilt
          , "startTime" .= (floor (realToFrac (diffUTCTime startTime (posixSecondsToUTCTime 0)) :: Double) :: Integer)
          , "stopTime" .= (floor (realToFrac (diffUTCTime stopTime (posixSecondsToUTCTime 0)) :: Double) :: Integer)
          ]

instance FromJSON BuildResult where
  parseJSON = withObject "BuildResult" $ \obj -> do
    success <- obj .: "success"
    timesBuilt <- obj .: "timesBuilt"
    startTimeSeconds <- obj .: "startTime"
    stopTimeSeconds <- obj .: "stopTime"
    let startTime = posixSecondsToUTCTime (fromInteger startTimeSeconds)
        stopTime = posixSecondsToUTCTime (fromInteger stopTimeSeconds)

    buildResultStatus <- if success
      then do
        successStatus <- obj .: "status"
        builtOutputs <- obj .:? "builtOutputs" .!= mempty
        pure $ Right (BuildSuccess successStatus builtOutputs)
      else do
        failureStatus <- obj .: "status"
        errorMsg <- obj .: "errorMsg"
        isNonDeterministic <- obj .: "isNonDeterministic"
        pure $ Left (BuildFailure failureStatus errorMsg isNonDeterministic)

    buildResultCpuUser <- obj .:? "cpuUser"
    buildResultCpuSystem <- obj .:? "cpuSystem"

    pure BuildResult
      { buildResultStatus = buildResultStatus
      , buildResultTimesBuilt = timesBuilt
      , buildResultStartTime = startTime
      , buildResultStopTime = stopTime
      , buildResultCpuUser = buildResultCpuUser
      , buildResultCpuSystem = buildResultCpuSystem
      }

-- | Metadata (path-info) JSON, version 3 format
instance ToJSON (Metadata StorePath) where
  toJSON m = object
    [ "version" .= (3 :: Int)
    , "storeDir" .= ("/nix/store" :: Text)
    , "narHash" .= HashJSON (metadataNarHash m)
    , "narSize" .= fromMaybe 0 (metadataNarBytes m)
    , "references" .= metadataReferences m
    , "ca" .= metadataContentAddress m
    , "deriver" .= metadataDeriverPath m
    , "registrationTime" .=
        let t = metadataRegistrationTime m
        in if t == posixSecondsToUTCTime 0
           then Nothing @Integer
           else Just (floor (realToFrac (diffUTCTime t (posixSecondsToUTCTime 0)) :: Double) :: Integer)
    , "signatures" .= metadataSigs m
    , "ultimate" .= (metadataTrust m == BuiltLocally)
    ]

instance FromJSON (Metadata StorePath) where
  parseJSON = withObject "Metadata" $ \o -> do
    HashJSON narHash <- o .: "narHash"
    narSize <- o .: "narSize"
    references <- o .:? "references" .!= mempty
    ca <- o .:? "ca"
    deriver <- o .:? "deriver"
    regTime <- o .:? "registrationTime"
    sigs <- o .:? "signatures" .!= mempty
    ultimate <- o .:? "ultimate" .!= False
    pure Metadata
      { metadataDeriverPath = deriver
      , metadataNarHash = narHash
      , metadataReferences = references
      , metadataRegistrationTime =
          maybe (posixSecondsToUTCTime 0) (posixSecondsToUTCTime . fromInteger) regTime
      , metadataNarBytes = if narSize == (0 :: Int) then Nothing else Just (fromIntegral narSize)
      , metadataTrust = if ultimate then BuiltLocally else BuiltElsewhere
      , metadataSigs = sigs
      , metadataContentAddress = ca
      }
