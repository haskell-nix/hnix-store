{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Aeson.KeyMap qualified
import Data.Aeson.Types (Parser)
import Data.Aeson.Types qualified
import Data.Attoparsec.Text qualified
import Data.Char qualified
import Data.Constraint.Extras (Has(has))
import Data.Default.Class
import Data.Dependent.Sum
import Data.Foldable (toList)
import Data.Map.Strict qualified
import Data.Map.Monoidal qualified
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified
import Data.Some
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Data.These
import Data.These.Combinators
import Data.Time (diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Deriving.Aeson
import GHC.Generics

import System.Nix.Base (baseEncodingToText, textToBaseEncoding)
import System.Nix.Base qualified
import System.Nix.Build (BuildResult(..), BuildSuccess(..), BuildFailure(..), BuildSuccessStatus(..), BuildFailureStatus(..))
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.Hash
import System.Nix.OutputName (OutputName)
import System.Nix.OutputName qualified
import System.Nix.Realisation (BuildTraceKey(..), Realisation, RealisationWithId(..))
import System.Nix.Realisation qualified
import System.Nix.Signature (Signature)
import System.Nix.Signature qualified
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart, mkStorePathName, unStorePathName, parseBasePathFromText)
import System.Nix.StorePath qualified

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
        (fail . show @System.Nix.StorePath.InvalidPathError)
        pure
    . parseBasePathFromText
    )

instance ToJSONKey StorePath where
  toJSONKey = Data.Aeson.Types.toJSONKeyText System.Nix.StorePath.storePathBaseToText

instance FromJSONKey StorePath where
  fromJSONKey = FromJSONKeyTextParser $
    either (fail . show @System.Nix.StorePath.InvalidPathError) pure . parseBasePathFromText

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

instance FromJSON FreeformDerivationOutput where
  parseJSON = withObject "FreeformDerivationOutput" $ \obj ->
    FreeformDerivationOutput
      <$> obj .:? "path"
      <*> (((,) <$> (obj .:? "method") <*> (obj .:? "hashAlgo")) >>= \case
        (Nothing, Nothing) -> pure Nothing
        (Just methodS, Just hashAlgoS) -> do
          method <- either fail pure $ textToMethod methodS
          Some hashAlgo <- either fail pure $ textToAlgo hashAlgoS
          hash <- obj .:? "hash" >>= \case
            Nothing -> pure Nothing
            Just hashS -> either fail pure $ has @NamedAlgo hashAlgo $
              Just <$> decodeDigestWith Base16 hashS
          pure $ Just (method, hashAlgo :=> Comp1 hash)
        (Nothing, Just _) -> fail "Cannot have \"hashAlgo\" without \"method\""
        (Just _, Nothing) -> fail "Cannot have \"method\" without \"hashAlgo\""
      )

instance ToJSON FreeformDerivationOutput where
  toJSON (FreeformDerivationOutput mPath mContentAddressing ) =
    object $ concat $
      [ maybeToList $ ("path" .=) <$> mPath
      , flip (maybe []) mContentAddressing $ \(method, hashAlgo :=> Comp1 mHash) -> concat
        [ ["method" .= methodToText method]
        , ["hashAlgo" .= algoToText hashAlgo]
        , maybeToList $ ("hash" .=) . encodeDigestWith Base16 <$> mHash
        ]
      ]

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
    inputs <- DerivationInputs
      <$> v .: "inputSrcs"
      <*> v .: "inputDrvs"
    Derivation name
      <$> (toSpecificOutputs def name =<< v .: "outputs")
      <*> pure inputs
      <*> v .: "system"
      <*> v .: "builder"
      <*> v .: "args"
      <*> v .: "env"

instance ToJSON Derivation where
  toJSON (Derivation name outputs (DerivationInputs inputSrcs inputDrvs) system builder args env) =
    object [ "name" .= name
           , "outputs" .= fromSpecificOutputs def name outputs
           , "inputSrcs" .= inputSrcs
           , "inputDrvs" .= inputDrvs
           , "system" .= system
           , "builder" .= builder
           , "args" .= args
           , "env" .= env
           ]


instance ToJSON (BuildTraceKey OutputName) where
  toJSON =
    toJSON
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.buildTraceKeyBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

  toEncoding =
    toEncoding
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.buildTraceKeyBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

instance ToJSONKey (BuildTraceKey OutputName) where
  toJSONKey =
    Data.Aeson.Types.toJSONKeyText
    $ Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.buildTraceKeyBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

instance FromJSON (BuildTraceKey OutputName) where
  parseJSON =
    withText "BuildTraceKey OutputName"
    ( either
        (fail . show)
        pure
    . System.Nix.Realisation.buildTraceKeyParser
        System.Nix.OutputName.mkOutputName
    )

instance FromJSONKey (BuildTraceKey OutputName) where
  fromJSONKey =
    FromJSONKeyTextParser
    ( either
        (fail . show)
        pure
    . System.Nix.Realisation.buildTraceKeyParser
        System.Nix.OutputName.mkOutputName
    )

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

-- | Needed to avoid overlapping instances
newtype HashJSON = HashJSON { unHashJSON :: DSum HashAlgo Digest }
  deriving (Eq, Show)

instance ToJSON HashJSON where
  toJSON (HashJSON (algo :=> digest)) =
    object
      [ "algorithm" .= algoToText algo
      , "format" .= baseEncodingToText Base64  -- Default to base64 for output
      , "hash" .= encodeDigestWith Base64 digest
      ]

instance FromJSON HashJSON where
  parseJSON = withObject "HashJSON" $ \obj -> do
    algoText <- obj .: "algorithm"
    formatText <- obj .: "format"
    hashText <- obj .: "hash"

    Some algo <- either fail pure $ textToAlgo algoText
    format <- either fail pure $ textToBaseEncoding formatText

    digest <- has @NamedAlgo algo $ either fail pure $ decodeDigestWith format hashText

    pure $ HashJSON (algo :=> digest)

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

data LowerLeading
instance StringModifier LowerLeading where
  getStringModifier "" = ""
  getStringModifier (c:xs) = Data.Char.toLower c : xs

deriving
  via CustomJSON
    '[FieldLabelModifier
       '[ StripPrefix "realisation"
        , LowerLeading
        , Rename "dependencies" "dependentRealisations"
        ]
     ] Realisation
  instance ToJSON Realisation
deriving
  via CustomJSON
    '[FieldLabelModifier
       '[ StripPrefix "realisation"
        , LowerLeading
        , Rename "dependencies" "dependentRealisations"
        ]
     ] Realisation
  instance FromJSON Realisation

-- For a keyed version of Realisation
-- we use RealisationWithId (BuildTraceKey OutputName, Realisation)
-- instead of Realisation.id :: (BuildTraceKey OutputName)
-- field.
instance ToJSON RealisationWithId where
  toJSON (RealisationWithId (drvOut, r)) =
    case toJSON r of
      Object o -> Object $ Data.Aeson.KeyMap.insert "id" (toJSON drvOut) o
      _ -> error "absurd"

instance FromJSON RealisationWithId where
  parseJSON v@(Object o) = do
    r <- parseJSON @Realisation v
    drvOut <- o .: "id"
    pure (RealisationWithId (drvOut, r))
  parseJSON x = fail $ "Expected Object but got " ++ show x

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
        -- Convert Map (BuildTraceKey OutputName) Realisation to Map OutputName RealisationWithId
        let builtOutputsForJSON = Data.Map.Strict.fromList
              [ (outName, RealisationWithId (btk, r))
              | (btk@(BuildTraceKey _hash outName), r) <- Data.Map.Strict.toList builtOutputs
              ]
        in object $ concat
          [ [ "success" .= True
            , "status" .= successStatus
            , "builtOutputs" .= builtOutputsForJSON
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
        -- Parse as Map OutputName RealisationWithId, then convert to Map (BuildTraceKey OutputName) Realisation
        builtOutputsWithId <- obj .: "builtOutputs" :: Parser (Data.Map.Strict.Map OutputName RealisationWithId)
        let builtOutputs = Data.Map.Strict.fromList
              [ (btk, r)
              | (_outName, RealisationWithId (btk, r)) <- Data.Map.Strict.toList builtOutputsWithId
              ]
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
