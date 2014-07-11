{- |
Module      : Nix.Store
Description : The nix store API
Copyright   : Copyright (c) 2014 Shea Levy
License     : MIT
Maintainer  : shea@shealevy.com
Stability   : Experimental
Portability : Portable

The API for operations on the nix store.
-}
module Nix.Store where

import Nix.Types (PathSet)

-- * The @Store@ class

-- | The class of implementations of the nix store API
class Store a where
    -- | Check whether a path is valid.
    isValidPath :: a         -- ^ The store
                -> FilePath  -- ^ The path to check
                -> IO Bool   -- ^ Whether the path is valid
    -- | Check which paths are valid.
    queryValidPaths :: a           -- ^ The store
                    -> PathSet     -- ^ The paths to check
                    -> IO PathSet  -- ^ The paths that are valid
