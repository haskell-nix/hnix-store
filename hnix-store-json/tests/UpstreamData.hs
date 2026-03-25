module UpstreamData
  ( parsesUpstream
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, it, shouldBe)

-- | Helper to test parsing upstream Nix test data files.
parsesUpstream :: (Eq a, Show a, FromJSON a) => FilePath -> FilePath -> a -> Spec
parsesUpstream dir filename expected =
  it ("parses " <> filename) $ do
    path <- getDataFileName (dir ++ "/" ++ filename)
    json <- BSL.readFile path
    eitherDecode json `shouldBe` Right expected
