{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StorePathSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.JSON ()
import System.Nix.StorePath (StorePath, parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/store-path/simple.json
upstreamSimple :: StorePath
upstreamSimple = forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    parsesUpstream "upstream-libstore-data/store-path" "simple.json" upstreamSimple
