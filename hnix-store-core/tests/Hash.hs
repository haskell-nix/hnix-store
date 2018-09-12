{-# LANGUAGE OverloadedStrings #-}

module Hash where

import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable
import           Data.Semigroup
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           System.Nix.Hash

spec_hashBase32truncateParity :: Spec
spec_hashBase32truncateParity = describe "hashBase32" $
  for_ testCases $ \(testCase, expectation) ->
    it ("computes correct base32 hash for string " <> BSC.unpack testCase) $
    hashToBase32 (hash testCase) `shouldBe` expectation
  where
    testCases :: [(BSC.ByteString, BSC.ByteString)]
    testCases =
      [ ("hello", "hcv22wi9b082i6qy160jgi9cvw3am153") ]
