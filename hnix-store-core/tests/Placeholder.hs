{-# LANGUAGE OverloadedStrings #-}

module Placeholder where

import Data.Text (Text)


-- import System.Nix.DerivedPath
import System.Nix.OutputName
import System.Nix.Placeholder
import System.Nix.StorePath
import Test.Hspec

spec_hash :: Spec
spec_hash =

  describe "placeholder" $ do

    it "output called 'object'" $ do

      let objectPlaceholder = "/07fvp8gkd5mhhfi1lqjfwq7sxnpmdfczz27lizfxiz6fpwad8sy4"
      shouldBe (renderPlaceholder $ createPlaceholder (OutputName $ bad "object"))
               objectPlaceholder
    it "output called 'interface'" $ do

      let interfacePlaceholder = "/1ang7n5l91vn079693l42ahmcxgf34r0qad1l01y4lf7d3cwm5lg"
      shouldBe (renderPlaceholder $ createPlaceholder (OutputName $ bad "interface"))
               interfacePlaceholder

bad :: Text -> StorePathName
bad = either (error . show) id . mkStorePathName


-- | Hash encoding conversion ground-truth.
-- Similiar to nix/tests/hash.sh
spec_nixhash :: Spec
spec_nixhash =


  describe "downstream placeholder" $ do
    it "known derivation, unknown output store path cause CA" $ do
      Right p <- pure $ parsePathFromText (StoreDir "/nix/store") "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"
      let dp = renderDownstreamPlaceholder $ unknownCaOutput p (OutputName $ bad "out")
      shouldBe dp "/0c6rn30q4frawknapgwq386zq358m8r6msvywcvc89n6m5p2dgbz"

    it "unknown derivation" $ do
      Right p <- pure $ parsePathFromText (StoreDir "/nix/store") "/nix/store/g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv.drv"
      let dp = renderDownstreamPlaceholder $ unknownDerivation (unknownCaOutput p (OutputName $ bad "out")) (OutputName $ bad "out")
      shouldBe dp "/0gn6agqxjyyalf0dpihgyf49xq5hqxgw100f0wydnj6yqrhqsb3w"

