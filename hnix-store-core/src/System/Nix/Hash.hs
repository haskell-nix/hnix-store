{-|
Description : Cryptographic hashes for hnix-store.
-}
module System.Nix.Hash
  ( Hash.Digest

  , Hash.HashAlgorithm(..)
  , Hash.ValidAlgo(..)
  , Hash.NamedAlgo(..)
  , Hash.SomeNamedDigest(..)

  , Hash.hash
  , Hash.hashLazy
  , Hash.mkNamedDigest

  , Hash.BaseEncoding(..)
  , Hash.encodeInBase
  , Hash.decodeBase
  )
where

import qualified System.Nix.Internal.Hash      as Hash
