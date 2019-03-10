{-|
Description : Cryptographic hashes for hnix-store.
-}
module System.Nix.Hash (
    HNix.Digest

  , HNix.HashAlgorithm
  , HNix.HashAlgorithm'(..)
  , HNix.AlgoVal(..)
  , HNix.HasDigest(..)
  , HNix.hash
  , HNix.hashLazy

  , HNix.printAsBase32
  ) where

import qualified System.Nix.Internal.Hash as HNix
