{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text.Lazy (Result(..))
import Data.Attoparsec.Text.Lazy qualified
import Data.Text.Lazy.IO qualified
import Text.Show.Pretty qualified

import System.Nix.StorePath
import System.Nix.Derivation.ATerm qualified

main :: IO ()
main = do
    text <- Data.Text.Lazy.IO.getContents
    case
      Data.Attoparsec.Text.Lazy.parse
        (System.Nix.Derivation.ATerm.parseDerivation
          (StoreDir "/nix/store")
          (error "todo get name from outputs if needed"))
        text
      of
        Fail _ _ err      -> fail err
        Done _ derivation -> Text.Show.Pretty.pPrint derivation
