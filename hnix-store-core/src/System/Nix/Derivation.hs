{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Derivation
  ( parseDerivation
  , buildDerivation
  )
where

import qualified Data.Text                     as Text
import qualified Data.Text.Lazy.Builder        as Text.Lazy
                                                ( Builder )
import qualified Data.Text.Lazy.Builder        as Text.Lazy.Builder
import qualified Data.Attoparsec.Text.Lazy     as Text.Lazy
                                                ( Parser )
import           Nix.Derivation                 ( Derivation )
import qualified Nix.Derivation                as Derivation
import           System.Nix.StorePath           ( StorePath )
import qualified System.Nix.StorePath          as StorePath



parseDerivation :: FilePath -> Text.Lazy.Parser (Derivation StorePath Text)
parseDerivation expectedRoot =
  Derivation.parseDerivationWith
    ("\"" *> StorePath.pathParser expectedRoot <* "\"")
    Derivation.textParser

buildDerivation :: Derivation StorePath Text -> Text.Lazy.Builder
buildDerivation =
  Derivation.buildDerivationWith
    (string . Text.pack . show)
    string
  where
    string = Text.Lazy.Builder.fromText . Text.pack . show
