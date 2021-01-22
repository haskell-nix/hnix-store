{-|
Description : Cryptographic hashes for hnix-store.
-}
module System.Nix.Hash (
    HNix.Digest

  , HNix.HashAlgorithm(..)
  , HNix.ValidAlgo(..)
  , HNix.NamedAlgo(..)
  , HNix.SomeNamedDigest(..)
  , HNix.hash
  , HNix.hashLazy
  , HNix.mkNamedDigest

  , HNix.BaseEncoding(..)
  , HNix.encodeInBase
  , HNix.decodeBase
  ) where

import qualified System.Nix.Internal.Old as HNix
