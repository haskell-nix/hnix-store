{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SingleDerivedPathSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.StorePath (parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/derived-path/single_opaque.json
upstreamSingleOpaque :: SingleDerivedPath
upstreamSingleOpaque = SingleDerivedPath_Opaque $
  forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

-- upstream-nix/src/libstore-tests/data/derived-path/single_built.json
upstreamSingleBuilt :: SingleDerivedPath
upstreamSingleBuilt = SingleDerivedPath_Built
  (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
  (forceRight $ System.Nix.OutputName.mkOutputName "bar")

-- upstream-nix/src/libstore-tests/data/derived-path/single_built_built.json
upstreamSingleBuiltBuilt :: SingleDerivedPath
upstreamSingleBuiltBuilt = SingleDerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  (forceRight $ System.Nix.OutputName.mkOutputName "baz")

spec :: Spec
spec = do
  let dir = "upstream-libstore-data/derived-path"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "single_opaque.json" upstreamSingleOpaque
    parsesUpstream dir "single_built.json" upstreamSingleBuilt
    parsesUpstream dir "single_built_built.json" upstreamSingleBuiltBuilt
