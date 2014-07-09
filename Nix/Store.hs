module Nix.Store where

class Store a where
  isValidPath :: a -> FilePath -> IO Bool
