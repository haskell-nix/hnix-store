{-|
Description : Representation of Nix store paths.
-}
module System.Nix.StorePath
  ( -- * Basic store path types
    StorePath(..)
  , StorePathName
  , StorePathHashAlgo
  , StoreDir
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , unStorePathName
  , storePathNameRegex
  , -- * Rendering out 'StorePath's
    storePathToRawFilePath
  , KnownStoreDir
  ) where

import System.Nix.Internal.StorePath
