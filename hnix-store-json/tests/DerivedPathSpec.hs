{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DerivedPathSpec where

import Data.Set qualified
import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.DerivedPath (DerivedPath(..), OutputsSpec(..), SingleDerivedPath(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified
import System.Nix.StorePath (parseBasePathFromText)

-- upstream-nix/src/libstore-tests/data/derived-path/multi_opaque.json
upstreamMultiOpaque :: DerivedPath
upstreamMultiOpaque = DerivedPath_Opaque $
  forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv"

-- upstream-nix/src/libstore-tests/data/derived-path/mutli_built.json
upstreamMultiBuilt :: DerivedPath
upstreamMultiBuilt = DerivedPath_Built
  (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
  (OutputsSpec_Names $ Data.Set.fromList
    [ forceRight $ System.Nix.OutputName.mkOutputName "bar"
    , forceRight $ System.Nix.OutputName.mkOutputName "baz"
    ])

-- upstream-nix/src/libstore-tests/data/derived-path/multi_built_built.json
upstreamMultiBuiltBuilt :: DerivedPath
upstreamMultiBuiltBuilt = DerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  (OutputsSpec_Names $ Data.Set.fromList
    [ forceRight $ System.Nix.OutputName.mkOutputName "baz"
    , forceRight $ System.Nix.OutputName.mkOutputName "quux"
    ])

-- upstream-nix/src/libstore-tests/data/derived-path/multi_built_built_wildcard.json
upstreamMultiBuiltBuiltWildcard :: DerivedPath
upstreamMultiBuiltBuiltWildcard = DerivedPath_Built
  (SingleDerivedPath_Built
    (SingleDerivedPath_Opaque $ forceRight $ parseBasePathFromText "g1w7hy3qg1w7hy3qg1w7hy3qg1w7hy3q-foo.drv")
    (forceRight $ System.Nix.OutputName.mkOutputName "bar"))
  OutputsSpec_All

spec :: Spec
spec = do
  let dir = "upstream-libstore-data/derived-path"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "multi_opaque.json" upstreamMultiOpaque
    parsesUpstream dir "mutli_built.json" upstreamMultiBuilt
    parsesUpstream dir "multi_built_built.json" upstreamMultiBuiltBuilt
    parsesUpstream dir "multi_built_built_wildcard.json" upstreamMultiBuiltBuiltWildcard
