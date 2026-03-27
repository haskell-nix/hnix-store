module SignatureSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.Nix (roundtrips)
import Test.Hspec.QuickCheck (prop)

import System.Nix.Signature (signatureToText, parseSignature, narSignatureToText, parseNamedSignature)
import System.Nix.Arbitrary ()

spec :: Spec
spec = do
  describe "Signature" $ do
    prop "roundtrips" $ roundtrips signatureToText parseSignature
  describe "NamedSignature" $ do
    prop "roundtrips" $ roundtrips narSignatureToText parseNamedSignature
