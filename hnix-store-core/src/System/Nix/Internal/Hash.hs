{-|
Description : Cryptographic hashing interface for hnix-store, on top
              of the cryptohash family of libraries.
-}
{-# LANGUAGE PackageImports #-}

module System.Nix.Internal.Hash
 ( H.hash
 , H.hashlazy
 , H.HashAlgorithm(..)
 ) where

import "cryptonite" Crypto.Hash as H
import "cryptonite" Crypto.Hash.Algorithms as H



