{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OutputsSpecSpec where

import Data.Set qualified
import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.DerivedPath (OutputsSpec(..))
import System.Nix.JSON ()
import System.Nix.OutputName qualified

-- upstream-nix/src/libstore-tests/data/outputs-spec/all.json
upstreamAll :: OutputsSpec
upstreamAll = OutputsSpec_All

-- upstream-nix/src/libstore-tests/data/outputs-spec/name.json
upstreamName :: OutputsSpec
upstreamName = OutputsSpec_Names $
  Data.Set.singleton $ forceRight $ System.Nix.OutputName.mkOutputName "a"

-- upstream-nix/src/libstore-tests/data/outputs-spec/names.json
upstreamNames :: OutputsSpec
upstreamNames = OutputsSpec_Names $ Data.Set.fromList
  [ forceRight $ System.Nix.OutputName.mkOutputName "a"
  , forceRight $ System.Nix.OutputName.mkOutputName "b"
  ]

spec :: Spec
spec = do
  let dir = "upstream-libstore-data/outputs-spec"
  describe "upstream Nix test data" $ do
    parsesUpstream dir "all.json" upstreamAll
    parsesUpstream dir "name.json" upstreamName
    parsesUpstream dir "names.json" upstreamNames
