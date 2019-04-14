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
  , HNix.encodeBase32
  , HNix.encodeBase16
  ) where

import qualified System.Nix.Internal.Hash as HNix
