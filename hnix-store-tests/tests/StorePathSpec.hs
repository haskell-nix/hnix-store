module StorePathSpec where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

import System.Nix.Arbitrary ()
import System.Nix.StorePath

import qualified Data.Attoparsec.Text

spec :: Spec
spec = do
  describe "StorePath" $ do
    prop "roundtrips using parsePath . storePathToRawFilePath" $
      \storeDir x ->
        parsePath storeDir (storePathToRawFilePath storeDir x) `shouldBe` pure x

    prop "roundtrips using parsePathFromText . storePathToText" $
      \storeDir x ->
        parsePathFromText storeDir (storePathToText storeDir x) `shouldBe` pure x

    prop "roundtrips using pathParser . storePathToText" $
      \storeDir x ->
        Data.Attoparsec.Text.parseOnly
          (pathParser storeDir)
          (storePathToText storeDir x) `shouldBe` pure x
