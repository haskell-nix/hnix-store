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

import Crypto.Hash (Digest)
import Data.Aeson
import Data.Aeson.KeyMap qualified
import Data.Aeson.Types qualified
import Data.Attoparsec.Text qualified
import Data.Char qualified
import Data.Constraint.Extras (Has(has))
import Data.Default.Class
import Data.Dependent.Sum
import Data.Map.Monoidal qualified
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified
import Data.Some
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Data.These
import Data.These.Combinators
import Deriving.Aeson
import GHC.Generics

import System.Nix.Base (baseEncodingToText, textToBaseEncoding)
import System.Nix.Base qualified
import System.Nix.ContentAddress
import System.Nix.Derivation
import System.Nix.Hash
import System.Nix.OutputName (OutputName)
import System.Nix.OutputName qualified
import System.Nix.Realisation (BuildTraceKey, Realisation, RealisationWithId(..))
import System.Nix.Realisation qualified
import System.Nix.Signature (Signature)
import System.Nix.Signature qualified
import System.Nix.StorePath (StorePath, StorePathName, StorePathHashPart, storePathHash, storePathName, mkStorePathName, unStorePathName, parseBasePathFromText)
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
  toJSON sp =
    toJSON $ Data.Text.concat
      [ System.Nix.StorePath.storePathHashPartToText (storePathHash sp)
      , "-"
      , System.Nix.StorePath.unStorePathName (storePathName sp)
      ]

  toEncoding sp =
    toEncoding $ Data.Text.concat
      [ System.Nix.StorePath.storePathHashPartToText (storePathHash sp)
      , "-"
      , System.Nix.StorePath.unStorePathName (storePathName sp)
      ]

instance FromJSON StorePath where
  parseJSON =
    withText "StorePath"
    ( either
        (fail . show @System.Nix.StorePath.InvalidPathError)
        pure
    . parseBasePathFromText
    )

instance ToJSONKey StorePath where
  toJSONKey = Data.Aeson.Types.toJSONKeyText $ \sp ->
    Data.Text.concat
      [ System.Nix.StorePath.storePathHashPartToText (storePathHash sp)
      , "-"
      , System.Nix.StorePath.unStorePathName (storePathName sp)
      ]

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
