{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Default.Class (Default(def))

import Data.Text qualified
import System.Environment qualified
import System.Nix.Build qualified
import System.Nix.StorePath qualified
import System.Nix.Store.Remote qualified

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

