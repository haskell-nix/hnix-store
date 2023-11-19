module Main where

import qualified System.Nix.Store.DB.Run

main :: IO ()
main = System.Nix.Store.DB.Run.bench
