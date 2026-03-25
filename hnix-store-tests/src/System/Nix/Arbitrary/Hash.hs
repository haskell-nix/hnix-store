{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- due to recent generic-arbitrary
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module System.Nix.Arbitrary.Hash where

import Data.ByteString (ByteString)
import Data.ByteString.Arbitrary ()
import Data.Constraint.Extras
import Crypto.Hash (Digest, MD5(..), SHA1(..), SHA256(..), SHA512(..))
import Data.Dependent.Sum (DSum((:=>)))
import Data.Some (Some(Some))
import GHC.Generics

import System.Nix.Hash (HashAlgo(..))

import Test.QuickCheck (Arbitrary(arbitrary), oneof)

import Crypto.Hash qualified

-- * Arbitrary @Digest@s

instance Arbitrary (Digest MD5) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA1) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA256) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Digest SHA512) where
  arbitrary = Crypto.Hash.hash @ByteString <$> arbitrary

instance Arbitrary (Some HashAlgo)  where
  arbitrary =
    oneof
    $ pure
    <$> [
      Some HashAlgo_MD5
    , Some HashAlgo_SHA1
    , Some HashAlgo_SHA256
    , Some HashAlgo_SHA512
    ]

-- TODO Upstream to dependent-sum or quickcheck-dependent

-- | Generate a dependent sum given generators for the tag and value.
-- Note: works for any @Monad m@, not just @Gen@.
genDSum :: Monad m => m (Some f) -> (forall a. f a -> m (g a)) -> m (DSum f g)
genDSum genTag genValue = genTag >>= \(Some tag) ->
  (tag :=>) <$> genValue tag

instance (Arbitrary (Some f), Has' Arbitrary f g) => Arbitrary (DSum f g)  where
  arbitrary = genDSum arbitrary (\tag -> has' @Arbitrary @g tag arbitrary)

instance (Arbitrary (f (g a))) => Arbitrary ((f :.: g) a)  where
  arbitrary = Comp1 <$> arbitrary
