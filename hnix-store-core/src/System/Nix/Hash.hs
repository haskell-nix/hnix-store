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

  , Base.BaseEncoding(..)
  , Hash.encodeDigestWith
  , Hash.decodeDigestWith
  )
where

import qualified System.Nix.Internal.Hash      as Hash
import qualified System.Nix.Internal.Base      as Base
