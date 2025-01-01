{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.HashSet.Arbitrary where

import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Test.QuickCheck (Arbitrary(..))
import Data.HashSet qualified

instance (Hashable a, Eq a, Arbitrary a) => Arbitrary (HashSet a) where
  arbitrary = Data.HashSet.fromList <$> arbitrary
  shrink hashset = Data.HashSet.fromList <$> shrink (Data.HashSet.toList hashset)
