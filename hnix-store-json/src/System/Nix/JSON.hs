{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description : JSON serialization

This module is mostly a stub for now
providing (From|To)JSON for Realisation type
which is required for `-remote`.
-}
module System.Nix.JSON where

import Data.Aeson
import Deriving.Aeson
import System.Nix.Base (BaseEncoding(NixBase32))
import System.Nix.OutputName (OutputName)
import System.Nix.Realisation (DerivationOutput, Realisation, RealisationWithId(..))
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StoreDir(..), StorePath, StorePathName, StorePathHashPart)

import Data.Aeson.KeyMap qualified
import Data.Aeson.Types qualified
import Data.Attoparsec.Text qualified
import Data.Char qualified
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.Base qualified
import System.Nix.OutputName qualified
import System.Nix.Realisation qualified
import System.Nix.Signature qualified
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
  toJSON =
    toJSON
    -- TODO: hacky, we need to stop requiring StoreDir for
    -- StorePath rendering and have a distinct
    -- types for rooted|unrooted paths
    . Data.Text.drop 1
    . System.Nix.StorePath.storePathToText (StoreDir mempty)

  toEncoding =
    toEncoding
    . Data.Text.drop 1
    . System.Nix.StorePath.storePathToText (StoreDir mempty)

instance FromJSON StorePath where
  parseJSON =
    withText "StorePath"
    ( either
        (fail . show)
        pure
    . System.Nix.StorePath.parsePathFromText (StoreDir mempty)
    . Data.Text.cons '/'
    )

instance ToJSON (DerivationOutput OutputName) where
  toJSON =
    toJSON
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        System.Nix.OutputName.unOutputName

  toEncoding =
    toEncoding
    . Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        System.Nix.OutputName.unOutputName

instance ToJSONKey (DerivationOutput OutputName) where
  toJSONKey =
    Data.Aeson.Types.toJSONKeyText
    $ Data.Text.Lazy.toStrict
    . Data.Text.Lazy.Builder.toLazyText
    . System.Nix.Realisation.derivationOutputBuilder
        System.Nix.OutputName.unOutputName

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
