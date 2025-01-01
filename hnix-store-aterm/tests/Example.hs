module Main where

import Nix.Derivation qualified
import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Data.Text.Lazy.IO qualified

import Data.Attoparsec.Text.Lazy (Result(..))

main :: IO ()
main = do
    text0 <- Data.Text.Lazy.IO.readFile "tests/example0.drv"
    derivation <- case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text0 of
        Fail _ _ string   -> fail string
        Done _ derivation -> return derivation
    let builder = Nix.Derivation.buildDerivation derivation
    let text1   = Data.Text.Lazy.Builder.toLazyText builder
    if text0 == text1
        then return ()
        else fail "Parsing and rendering the example derivation does not round-trip"
