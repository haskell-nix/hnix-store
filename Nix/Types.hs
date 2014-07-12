{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{- |
Module      : Nix.Types
Description : Miscellaneous types for the nix APIs
Copyright   : Copyright (c) 2014 Shea Levy
License     : MIT
Maintainer  : shea@shealevy.com
Stability   : Experimental
Portability : Portable
-}
module Nix.Types where

import qualified Data.ByteString as BS
-- !!! Consider TrieSet
import Data.HashSet (HashSet)

import Data.ByteString.Base16 (decode)

-- | A set of paths (typically store paths).
type PathSet = HashSet FilePath

-- | Possible types of cryptographic hashes
data HashType = Unknown | Md5 | Sha1 | Sha256 deriving (Show)

-- | Parse a string repr of a hash type
parseHashType :: BS.ByteString -> HashType
parseHashType s
    | s == "md5"    = Md5
    | s == "sha1"   = Sha1
    | s == "sha256" = Sha256
    | otherwise     = Unknown

-- | The size, in bytes, of a hash of a given type
hashSize :: Num a
         => HashType  -- ^ The type of hash. Must not be Unknown.
         -> a
hashSize Unknown  = error "Attempted to determine appropriate size of a hash of Unknown type"
hashSize Md5 = 16
hashSize Sha1 = 20
hashSize Sha256 = 32

-- | A cryptographic hash of a specific type
data Hash = Hash HashType BS.ByteString deriving (Show)

-- | Parse a hex repr of a given Hash
parseHash :: HashType       -- ^ The type of the hash
          -> BS.ByteString  -- ^ The hex bytestring. Must be the right length and all hex
          -> Hash
parseHash ht s = Hash ht decoded
  where
    (first, second) = decode s
    decoded
        | BS.null second && BS.length first == hashSize ht = first
        | otherwise = error $ "Invalid hash " ++ (show s) ++ " of type " ++ (show ht)
