module StorePathSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.StorePath

import Data.Attoparsec.Text qualified

spec :: Spec
spec = do
  describe "StorePath" $ do
    prop "roundtrips using parsePath . storePathToRawFilePath" $
      \storeDir ->
        roundtrips
          (storePathToRawFilePath storeDir)
          (parsePath storeDir)

    prop "roundtrips using parsePathFromText . storePathToText" $
      \storeDir ->
        roundtrips
          (storePathToText storeDir)
          (parsePathFromText storeDir)

    prop "roundtrips using pathParser . storePathToText" $
      \storeDir ->
        roundtrips
          (storePathToText storeDir)
          (Data.Attoparsec.Text.parseOnly $ pathParser storeDir)
