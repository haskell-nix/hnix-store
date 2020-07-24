{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Derivation (
    parseDerivation
  , buildDerivation
  ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Nix.Derivation (Derivation)
import System.Nix.StorePath (StorePath, pathParser)

import qualified Data.ByteString.Char8
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Attoparsec.Text.Lazy

import qualified Nix.Derivation
import qualified System.Nix.StorePath

parseDerivation :: FilePath -> Parser (Derivation StorePath Text)
parseDerivation expectedRoot =
  Nix.Derivation.parseDerivationWith
    ("\"" *> System.Nix.StorePath.pathParser expectedRoot <* "\"")
    Nix.Derivation.textParser

buildDerivation :: Derivation StorePath Text -> Builder
buildDerivation derivation =
  Nix.Derivation.buildDerivationWith
    (string . Data.Text.pack . show)
    string
    derivation
  where
    string = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show
