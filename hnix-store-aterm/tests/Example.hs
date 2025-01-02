{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Data.Text.Lazy.IO qualified
import Data.Attoparsec.Text.Lazy (Result(..))

import System.Nix.StorePath
import System.Nix.Derivation.ATerm qualified

main :: IO ()
main = do
    name <- either (fail . show) pure $ mkStorePathName "perl-MIME-Types-2.13-devdoc"
    let storeDir = StoreDir "/nix/store"
    text0 <- Data.Text.Lazy.IO.readFile "tests/example0.drv"
    derivation <-
      case
        Data.Attoparsec.Text.Lazy.parse
          (System.Nix.Derivation.ATerm.parseDerivation storeDir name)
          text0
      of
        Fail _ _ string   -> fail string
        Done _ derivation -> return derivation
    let builder = System.Nix.Derivation.ATerm.buildDerivation storeDir derivation
    let text1   = Data.Text.Lazy.Builder.toLazyText builder
    if text0 == text1
        then return ()
        else fail "Parsing and rendering the example derivation does not round-trip"
