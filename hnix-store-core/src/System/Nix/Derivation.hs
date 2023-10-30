
module System.Nix.Derivation
  ( parseDerivation
  , buildDerivation
  )
where

import qualified Data.Text.Lazy.Builder        as Text.Lazy
                                                ( Builder )
import qualified Data.Attoparsec.Text.Lazy     as Text.Lazy
                                                ( Parser )
import           Nix.Derivation                 ( Derivation )
import qualified Nix.Derivation                as Derivation
import           System.Nix.StorePath           ( StoreDir
                                                , StorePath
                                                , storePathToFilePath
                                                )
import qualified System.Nix.StorePath          as StorePath



parseDerivation :: StoreDir -> Text.Lazy.Parser (Derivation StorePath Text)
parseDerivation expectedRoot =
  Derivation.parseDerivationWith
    ("\"" *> StorePath.pathParser expectedRoot <* "\"")
    Derivation.textParser

buildDerivation :: StoreDir -> Derivation StorePath Text -> Text.Lazy.Builder
buildDerivation storeDir =
  Derivation.buildDerivationWith
    (show . storePathToFilePath storeDir)
    show
