{-# OPTIONS_GHC -Wno-orphans #-}
-- Stolen from quickcheck-instances (BSD-3)
module Data.Vector.Arbitrary () where

import Data.Vector (Vector)
import Test.QuickCheck (Arbitrary(..), Arbitrary1(..), arbitrary1, shrink1)
import qualified Data.Vector

instance Arbitrary1 Vector where
  liftArbitrary =
    fmap Data.Vector.fromList
    . liftArbitrary
  liftShrink shr =
    fmap Data.Vector.fromList
    . liftShrink shr
    . Data.Vector.toList

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = arbitrary1
  shrink = shrink1
