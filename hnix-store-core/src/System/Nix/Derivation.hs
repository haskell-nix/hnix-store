module System.Nix.Derivation
  ( parseDerivation
  , buildDerivation
  -- Re-exports
  , Derivation(..)
  , DerivationOutput(..)
  ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)

import Nix.Derivation (Derivation(..), DerivationOutput(..))
import System.Nix.StorePath (StoreDir, StorePath)

import Data.Attoparsec.Text.Lazy qualified
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified

import Nix.Derivation qualified
import System.Nix.StorePath qualified

parseDerivation :: StoreDir -> Parser (Derivation StorePath Text)
parseDerivation expectedRoot =
  Nix.Derivation.parseDerivationWith
    pathParser
    Nix.Derivation.textParser
  where
    pathParser = do
      text <- Nix.Derivation.textParser
      case Data.Attoparsec.Text.Lazy.parseOnly
            (System.Nix.StorePath.pathParser expectedRoot)
            (Data.Text.Lazy.fromStrict text)
        of
          Right p -> pure p
          Left e -> fail e

buildDerivation :: StoreDir -> Derivation StorePath Text -> Builder
buildDerivation storeDir =
  Nix.Derivation.buildDerivationWith
    (string . System.Nix.StorePath.storePathToText storeDir)
    string
  where
    string = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show
