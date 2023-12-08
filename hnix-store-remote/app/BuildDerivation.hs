{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Default.Class (Default(def))

import qualified Data.Text
import qualified System.Environment
import qualified System.Nix.Build
import qualified System.Nix.StorePath
import qualified System.Nix.Store.Remote

main :: IO ()
main = System.Environment.getArgs >>= \case
  [filename] -> do
    case System.Nix.StorePath.parsePathFromText def (Data.Text.pack filename) of
      Left e -> error $ show e
      Right p -> do
        out <-
            System.Nix.Store.Remote.runStore
          $ System.Nix.Store.Remote.buildDerivation
              p
              System.Nix.Build.BuildMode_Normal
        print out
  _ -> error "No input derivation file"

