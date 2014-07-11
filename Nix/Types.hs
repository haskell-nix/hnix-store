{- |
Module      : Nix.Types
Description : Miscellaneous types for the nix APIs
Copyright   : Copyright (c) 2014 Shea Levy
License     : MIT
Maintainer  : shea@shealevy.com
Stability   : Experimental
Portability : Portable
-}
module Nix.Types where

-- !!! Consider TrieSet
import Data.HashSet (HashSet)

-- | A set of paths (typically store paths).
type PathSet = HashSet FilePath
