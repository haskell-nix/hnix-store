{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Default.Class (Default(def))
import Data.Text (Text)
import System.Nix.Derivation (Derivation)
import System.Nix.StorePath (StorePath)

import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Attoparsec.Text
import qualified System.Environment
import qualified System.Nix.Build
import qualified System.Nix.Derivation
import qualified System.Nix.StorePath
import qualified System.Nix.Store.Remote

parseDerivation :: FilePath -> IO (Derivation StorePath Text)
parseDerivation source = do
  contents <- Data.Text.IO.readFile source
  case Data.Attoparsec.Text.parseOnly
    (System.Nix.Derivation.parseDerivation def) contents of
      Left e -> error e
      Right drv -> pure drv

main :: IO ()
main = System.Environment.getArgs >>= \case
  [filename] -> do
    case System.Nix.StorePath.parsePathFromText def (Data.Text.pack filename) of
      Left e -> error $ show e
      Right p -> do
        d <- parseDerivation filename
        out <-
            System.Nix.Store.Remote.runStore
          $ System.Nix.Store.Remote.buildDerivation
              p
              d
              System.Nix.Build.BuildMode_Normal
        print out
  _ -> error "No input derivation file"

