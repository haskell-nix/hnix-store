{-|
Description : Representation of Nix store paths.
-}
module System.Nix.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , StorePath(..)
  , StorePathName
  , StorePathSet
  , StorePathHashPart
  , mkStorePathHashPart
  , unStorePathHashPart
  , ContentAddressableAddress(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , unStorePathName
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
