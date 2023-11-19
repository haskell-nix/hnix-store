-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Hash where

import Data.ByteString (ByteString)
import Crypto.Hash (Digest, MD5(..), SHA1(..), SHA256(..), SHA512(..))
import Data.Dependent.Sum (DSum((:=>)))
import System.Nix.Hash (HashAlgo(..))

import Test.QuickCheck (Arbitrary(arbitrary), oneof)
import Test.QuickCheck.Instances ()

import qualified Crypto.Hash

-- * Arbitrary @Digest@s

instance Arbitrary (Digest MD5) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA1) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA256) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA512) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

-- * Arbitrary @DSum HashAlgo Digest@s

instance Arbitrary (DSum HashAlgo Digest)  where
  arbitrary = oneof
    [ (HashAlgo_MD5 :=>)    <$> arbitrary
    , (HashAlgo_SHA1 :=>)   <$> arbitrary
    , (HashAlgo_SHA256 :=>) <$> arbitrary
    , (HashAlgo_SHA512 :=>) <$> arbitrary
    ]
