module SignatureSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (roundtrips)
import Test.Hspec.QuickCheck (prop)

import System.Nix.Signature (signatureToText, parseSignature, narSignatureToText, parseNarSignature)
import System.Nix.Arbitrary ()

spec :: Spec
spec = do
  describe "Signature" $ do
    prop "roundtrips" $ roundtrips signatureToText parseSignature
  describe "NarSignature" $ do
    prop "roundtrips" $ roundtrips narSignatureToText parseNarSignature
