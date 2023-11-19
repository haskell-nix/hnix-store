module BaseEncodingSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Base
import System.Nix.Arbitrary ()
import System.Nix.StorePath (StorePathHashPart(..))

spec :: Spec
spec = do
  describe "Hash" $ do
    prop "Base16 roundtrips" $
      roundtrips
        (encodeWith Base16)
        (decodeWith Base16)
        . unStorePathHashPart

    prop "Nix-like Base32 roundtrips" $
      roundtrips
        (encodeWith NixBase32)
        (decodeWith NixBase32)
        . unStorePathHashPart

    prop "Base64 roundtrips" $
      roundtrips
        (encodeWith Base64)
        (decodeWith Base64)
        . unStorePathHashPart
