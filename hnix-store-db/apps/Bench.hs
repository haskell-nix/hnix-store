module Main where

import System.Nix.Store.DB.Run qualified

main :: IO ()
main = System.Nix.Store.DB.Run.bench
