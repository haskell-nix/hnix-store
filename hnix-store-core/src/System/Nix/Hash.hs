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

  , HNix.encodeBase32
  , HNix.decodeBase32
  , HNix.encodeBase16
  , HNix.decodeBase16
  ) where

import qualified System.Nix.Internal.Hash as HNix
