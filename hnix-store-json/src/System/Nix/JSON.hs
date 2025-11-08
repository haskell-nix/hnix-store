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
import Data.Aeson.Key qualified
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
import Data.Time (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Deriving.Aeson
import GHC.Generics

import System.Nix.Base (baseEncodingToText, textToBaseEncoding)
import System.Nix.Base qualified
import System.Nix.ContentAddress
import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.DerivedPath qualified
import System.Nix.Hash
import System.Nix.OutputName (OutputName)
import System.Nix.OutputName qualified
import System.Nix.Realisation (DerivationOutput(..), Realisation, RealisationWithId(..))
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

instance FromJSONKey StorePathName where
  fromJSONKey = FromJSONKeyTextParser $ either (fail . show) pure . mkStorePathName

instance ToJSONKey StorePathName where
  toJSONKey = Data.Aeson.Types.toJSONKeyText unStorePathName

deriving newtype instance FromJSON OutputName
deriving newtype instance ToJSON OutputName
deriving newtype instance FromJSONKey OutputName
deriving newtype instance ToJSONKey OutputName

instance ToJSON (DerivationOutput OutputName) where
  toJSON =
    toJSON
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

  toEncoding =
    toEncoding
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

instance ToJSONKey (DerivationOutput OutputName) where
  toJSONKey =
    Data.Aeson.Types.toJSONKeyText
    $ Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)

instance FromJSON (DerivationOutput OutputName) where
  parseJSON =
    withText "DerivationOutput OutputName"
    ( either
        (fail . show)
        pure
    . System.Nix.Realisation.derivationOutputParser
        System.Nix.OutputName.mkOutputName
    )

instance FromJSONKey (DerivationOutput OutputName) where
  fromJSONKey =
    FromJSONKeyTextParser
    ( either
        (fail . show)
        pure
    . System.Nix.Realisation.derivationOutputParser
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
    case outputs of
      OutputsSpec_Names names | Data.Set.size names == 1 ->
        object
          [ "drvPath" .= toJSON drvPath
          , "output" .= Data.Set.elemAt 0 names
          ]
      _ ->
        object
          [ "drvPath" .= toJSON drvPath
          , "outputs" .= outputs
          ]

instance FromJSON DerivedPath where
  parseJSON v = parseOpaque v <|> parseBuilt v
    where
      parseOpaque = fmap DerivedPath_Opaque . parseJSON
      parseBuilt = withObject "DerivedPath_Built" $ \obj -> do
        drvPath <- obj .: "drvPath"
        -- Try single output first, then multiple outputs
        mOutput <- obj .:? "output"
        mOutputs <- obj .:? "outputs"
        case (mOutput, mOutputs) of
          (Just output, Nothing) ->
            pure $ DerivedPath_Built drvPath (OutputsSpec_Names (Data.Set.singleton output))
          (Nothing, Just outputs) ->
            pure $ DerivedPath_Built drvPath outputs
          _ -> fail "Expected either 'output' or 'outputs' field"

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
-- we use RealisationWithId (DerivationOutput OutputName, Realisation)
-- instead of Realisation.id :: (DerivationOutput OutputName)
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
