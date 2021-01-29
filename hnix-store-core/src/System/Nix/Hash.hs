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
  , SRI.mkNamedDigest

  , B.BaseEncoding(..)
  , B.encodeInBase
  , B.decodeBase
  ) where

import qualified System.Nix.Internal.Old as HNix
import qualified System.Nix.Internal.Base as B
import qualified System.Nix.Internal.SriHash as SRI
