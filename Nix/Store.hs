module Nix.Store where

-- | The API to interface with the nix store.
class Store a where
    -- | Check whether a path is valid.
    isValidPath :: a         -- ^ The store
                -> FilePath  -- ^ The path to check
                -> IO Bool
