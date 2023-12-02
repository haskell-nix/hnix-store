{-# OPTIONS_GHC -Wno-orphans #-}
-- Stolen from quickcheck-instances (BSD-3)
-- UTCTime/DiffTime slightly modified to produce
-- values rounded to whole seconds
module System.Nix.Arbitrary.UTCTime where

import Data.Time (Day(..), DiffTime, UTCTime(..))
import Test.QuickCheck (Arbitrary(..))

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (ModifiedJulianDay <$>) . shrink . Data.Time.toModifiedJulianDay

instance Arbitrary DiffTime where
    -- without abs something weird happens, try it
    arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary =
        UTCTime
        <$> arbitrary
        <*> arbitrary
    shrink ut@(UTCTime day dayTime) =
         [ ut { Data.Time.utctDay     = d' } | d' <- shrink day     ]
      ++ [ ut { Data.Time.utctDayTime = t' } | t' <- shrink dayTime ]

