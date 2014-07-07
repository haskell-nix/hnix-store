module Nix.Store where

import System.FilePath (FilePath)

class Store a where
  isValidPath :: a -> FilePath -> IO Bool
