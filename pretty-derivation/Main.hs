module Main where

import Data.Attoparsec.Text.Lazy (Result(..))

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Nix.Derivation
import qualified Text.Show.Pretty

main :: IO ()
main = do
    text <- Data.Text.Lazy.IO.getContents
    case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text of
        Fail _ _ err      -> fail err
        Done _ derivation -> Text.Show.Pretty.pPrint derivation
