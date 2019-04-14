{-|
Description : Representation of Nix store paths.
-}
module System.Nix.StorePath
  ( -- * Basic store path types
    StorePath(..)
  , StorePathName
  , StorePathSet
  , StorePathHashAlgo
  , StoreDir
  , ContentAddressableAddress(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , unStorePathName
  , storePathNameRegex
  , -- * Rendering out 'StorePath's
    storePathToRawFilePath
  , storeDirVal
  , KnownStoreDir
  ) where

import System.Nix.Internal.StorePath
