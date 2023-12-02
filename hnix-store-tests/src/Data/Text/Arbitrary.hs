{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Text.Arbitrary () where

import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..))
import qualified Data.Text

instance Arbitrary Text where
  arbitrary = Data.Text.pack <$> arbitrary
  shrink xs = Data.Text.pack <$> shrink (Data.Text.unpack xs)
