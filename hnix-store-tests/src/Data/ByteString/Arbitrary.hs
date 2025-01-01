{-# OPTIONS_GHC -Wno-orphans #-}
module Data.ByteString.Arbitrary () where

import Data.ByteString (ByteString)
import Test.QuickCheck (Arbitrary(..))
import Data.ByteString.Char8 qualified

instance Arbitrary ByteString where
  arbitrary = Data.ByteString.Char8.pack <$> arbitrary
  shrink xs = Data.ByteString.Char8.pack <$> shrink (Data.ByteString.Char8.unpack xs)
