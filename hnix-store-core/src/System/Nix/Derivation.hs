module System.Nix.Derivation
  ( parseDerivation
  , buildDerivation
  , Derivation'
  -- Re-exports
  , Derivation(..)
  , DerivationOutput(..)
  , DerivationInputs(..)
  ) where

import Data.Attoparsec.Text.Lazy (Parser)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)

import Nix.Derivation
  ( Derivation(..)
  , DerivationOutput(..)
  , DerivationInputs(..)
  , parseDerivationOutputWith
  , parseDerivationInputsWith
  , buildDerivationOutputWith
  , buildDerivationInputsWith
  )
import System.Nix.StorePath (StoreDir, StorePath)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

import qualified Nix.Derivation
import qualified System.Nix.StorePath

type Derivation' = Derivation
  StorePath
  Text
  Text
  (DerivationOutput StorePath Text)
  (DerivationInputs StorePath Text)

parseDerivation
  :: StoreDir
  -> Parser (Derivation')
parseDerivation expectedRoot =
  Nix.Derivation.parseDerivationWith
    Nix.Derivation.textParser
    Nix.Derivation.textParser
    (parseDerivationOutputWith pathParser Nix.Derivation.textParser)
    (parseDerivationInputsWith pathParser Nix.Derivation.textParser)
  where
    pathParser = do
      text <- Nix.Derivation.textParser
      case Data.Attoparsec.Text.Lazy.parseOnly
            (System.Nix.StorePath.pathParser expectedRoot)
            (Data.Text.Lazy.fromStrict text)
        of
          Right p -> pure p
          Left e -> fail e

buildDerivation
  :: StoreDir
  -> Derivation'
  -> Builder
buildDerivation storeDir =
  Nix.Derivation.buildDerivationWith
    string
    string
    (buildDerivationOutputWith path string emptyString)
    (buildDerivationInputsWith path string)
  where
    emptyString = string Data.Text.empty
    path = string . System.Nix.StorePath.storePathToText storeDir
    string = Data.Text.Lazy.Builder.fromText . Data.Text.pack . show
