module Main where

import Data.Attoparsec.Text.Lazy (Result(..))

import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.IO qualified
import Nix.Derivation qualified
import Text.Show.Pretty qualified

main :: IO ()
main = do
    text <- Data.Text.Lazy.IO.getContents
    case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text of
        Fail _ _ err      -> fail err
        Done _ derivation -> Text.Show.Pretty.pPrint derivation
