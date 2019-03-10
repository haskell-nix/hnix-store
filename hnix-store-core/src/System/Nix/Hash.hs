{-|
Description : Cryptographic hashes for hnix-store.
-}
module System.Nix.Hash (
    HNix.Digest

  , HNix.HashAlgorithm(..)
  , HNix.HasDigest(..)
  , HNix.NamedAlgo(..)
  , HNix.hash
  , HNix.hashLazy

  , HNix.printAsBase32
  ) where

import qualified System.Nix.Internal.Hash as HNix
