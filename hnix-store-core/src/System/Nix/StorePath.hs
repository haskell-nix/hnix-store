{-|
Description : Representation of Nix store paths.
-}
module System.Nix.StorePath
  ( -- * Basic store path types
    StorePath(..)
  , StorePathName(..)
  , StorePathSet
  , mkStorePathHashPart
  , StorePathHashPart(..)
  , ContentAddressableAddress(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , validStorePathName
  , -- * Rendering out 'StorePath's
    storePathToFilePath
  , storePathToRawFilePath
  , storePathToText
  , storePathToNarInfo
  , -- * Parsing 'StorePath's
    parsePath
  , pathParser
  )
where

import           System.Nix.Internal.StorePath
