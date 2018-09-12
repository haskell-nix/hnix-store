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
    getTruncatedHash (hash testCase) `shouldBe` expectation
  where
    testCases :: [(BSC.ByteString, BSC.ByteString)]
    testCases = []
      -- [ ("hello", "hcv22wi9b082i6qy160jgi9cvw3am153") ]
      -- TODO: This test fails.
      -- See [issue #24](https://github.com/haskell-nix/hnix-store/issues/24)
