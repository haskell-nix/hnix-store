{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ContentAddressSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (forceRight)
import UpstreamData (parsesUpstream)

import System.Nix.ContentAddress (ContentAddress(..), ContentAddressMethod(..))
import System.Nix.Hash (mkNamedDigest)
import System.Nix.JSON ()

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    parsesUpstream "upstream-libstore-data/content-address" "nar.json" $
      ContentAddress
        ContentAddressMethod_NixArchive
        (forceRight $ mkNamedDigest "sha256" "9vLqj0XYoFfJVmoz+ZR02i5camYE1zYSFlDicwxvsKM=")

    parsesUpstream "upstream-libstore-data/content-address" "text.json" $
      ContentAddress
        ContentAddressMethod_Text
        (forceRight $ mkNamedDigest "sha256" "8OTC92xYkW7CWPJGhRvqCR0U1CR6L8PhhpRGGxgW4Ts=")
