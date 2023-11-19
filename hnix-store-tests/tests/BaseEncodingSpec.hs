module BaseEncodingSpec where

import Test.Hspec (Spec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, choose, listOf1, forAllShrink, genericShrink)

import System.Nix.Base
import System.Nix.Arbitrary ()
import System.Nix.StorePath (StorePathHashPart(..))
import qualified Data.ByteString.Char8
import qualified System.Nix.Base32

spec :: Spec
spec = do
  describe "Hash" $ do
    prop "Nix-like Base32 roundtrips" $
      -- TODO(srk): use decodeWith
      forAllShrink nonEmptyString genericShrink $ \x ->
        (System.Nix.Base32.decode
        . System.Nix.Base32.encode
        . Data.ByteString.Char8.pack $ x)
        `shouldBe`
        pure (Data.ByteString.Char8.pack x)
    prop "Base16 roundtrips" $ \x ->
      decodeWith Base16 (encodeWith Base16 $ unStorePathHashPart x)
      `shouldBe`
      pure (unStorePathHashPart x)
  where
    nonEmptyString :: Gen String
    nonEmptyString = listOf1 genSafeChar

    genSafeChar :: Gen Char
    genSafeChar = choose ('\1', '\127') -- ASCII without \NUL
