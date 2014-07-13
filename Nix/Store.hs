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

import Data.Word (Word)

import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime () -- UTCTime show instance

import Nix.Types (PathSet, Hash)

-- * Store API types

-- | Properties of a valid store path
data ValidPathInfo = ValidPathInfo
    { path             :: !FilePath          -- ^ The store path
    , deriver          :: !(Maybe FilePath)  -- ^ The derivation that created this path
    , hash             :: !Hash              -- ^ The hash of the path contents
    , references       :: !PathSet           -- ^ The other paths this path references
    , registrationTime :: !UTCTime           -- ^ When the path was originally registered
    , narSize          :: !(Maybe Word)      -- ^ The size of the path when serialized
    } deriving (Show)

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
    -- | Get a set of all valid paths.
    queryAllValidPaths :: a           -- ^ The store
                       -> IO PathSet  -- ^ All valid paths
    -- | Query information about a valid path.
    queryPathInfo :: a                 -- ^ The store
                  -> FilePath          -- ^ The path (must be valid)
                  -> IO ValidPathInfo  -- ^ The path properties

    -- | Get a set of all paths which reference a given path
    queryReferrers :: a           -- ^ The store
                   -> FilePath    -- ^ The path
                   -> IO PathSet  -- ^ The paths which reference the given path
