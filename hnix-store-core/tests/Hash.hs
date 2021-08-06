{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language CPP #-}

module Hash where

import qualified Data.ByteString.Char8       as BSC
import qualified Data.ByteString.Base16      as B16
import qualified System.Nix.Base32           as B32
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL

import           Test.Hspec
import           Test.Tasty.QuickCheck

import           System.Nix.Hash
import           System.Nix.StorePath
import           Arbitrary
import           System.Nix.Internal.Base
import           Crypto.Hash                ( MD5
                                            , SHA1
                                            , SHA256
                                            , hash
                                            )

spec_hash :: Spec
spec_hash = do

  describe "hashing parity with nix-store" $ do

    it "produces (base32 . sha256) of \"nix-output:foo\" the same as Nix does at the moment for placeholder \"foo\"" $
      shouldBe (encodeDigestWith NixBase32 (hash @ByteString @SHA256 "nix-output:foo"))
               "1x0ymrsy7yr7i9wdsqy9khmzc1yy7nvxw6rdp72yzn50285s67j5"
    it "produces (base16 . md5) of \"Hello World\" the same as the thesis" $
      shouldBe (encodeDigestWith Base16 (hash @ByteString @MD5 "Hello World"))
               "b10a8db164e0754105b7a99be72e3fe5"
    it "produces (base32 . sha1) of \"Hello World\" the same as the thesis" $
      shouldBe (encodeDigestWith NixBase32 (hash @ByteString @SHA1 "Hello World"))
               "s23c9fs0v32pf6bhmcph5rbqsyl5ak8a"

    -- The example in question:
    -- https://nixos.org/nixos/nix-pills/nix-store-paths.html
    it "produces same base32 as nix pill flat file example" $ do
      let exampleStr =
            "source:sha256:2bfef67de873c54551d884fdab3055d84d573e654efa79db3"
            <> "c0d7b98883f9ee3:/nix/store:myfile"
      shouldBe (encodeWith NixBase32 $ coerce $ mkStorePathHashPart exampleStr)
        "xv2iccirbrvklck36f1g7vldn5v58vck"

-- | Test that Nix-like base32 encoding roundtrips
prop_nixBase32Roundtrip :: Property
prop_nixBase32Roundtrip = forAllShrink nonEmptyString genericShrink $
  \x -> pure (BSC.pack x) === (B32.decode . B32.encode . BSC.pack $ x)

-- | API variants
prop_nixBase16Roundtrip :: StorePathHashPart -> Property
prop_nixBase16Roundtrip x = pure (coerce x) === decodeWith Base16 (encodeWith Base16 $ coerce x)

-- | Hash encoding conversion ground-truth.
-- Similiar to nix/tests/hash.sh
spec_nixhash :: Spec
spec_nixhash = do

  describe "hashing parity with nix-nash" $ do

    let
      samples = [
          ( "800d59cfcd3c05e900cb4e214be48f6b886a08df"
          , "vw46m23bizj4n8afrc0fj19wrp7mj3c0"
          , "gA1Zz808BekAy04hS+SPa4hqCN8="
          )
        , ( "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
          , "1b8m03r63zqhnjf7l5wnldhh7c134ap5vpj0850ymkq1iyzicy5s"
          , "ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0="
          )
        , ( "204a8fc6dda82f0a0ced7beb8e08a41657c16ef468b228a8279be331a703c33596fd15c13b1b07f9aa1d3bea57789ca031ad85c7a71dd70354ec631238ca3445"
          , "12k9jiq29iyqm03swfsgiw5mlqs173qazm3n7daz43infy12pyrcdf30fkk3qwv4yl2ick8yipc2mqnlh48xsvvxl60lbx8vp38yji0"
          , "IEqPxt2oLwoM7XvrjgikFlfBbvRosiioJ5vjMacDwzWW/RXBOxsH+aodO+pXeJygMa2Fx6cd1wNU7GMSOMo0RQ=="
          )
        ]

    it "b16 encoded . b32 decoded should equal original b16" $
      forM_ samples $ \(b16, b32, _b64) -> shouldBe (B16.encode <$> B32.decode b32) (Right b16)

    it "b64 encoded . b32 decoded should equal original b64" $
      forM_ samples $ \(_b16, b32, b64) -> shouldBe (B64.encode . BSL.fromStrict <$> B32.decode b32) (Right b64)

    it "b32 encoded . b64 decoded should equal original b32" $
      forM_ samples $ \(_b16, b32, b64) -> shouldBe (B32.encode . BSL.toStrict <$> B64.decode b64 ) (Right b32)

    it "b16 encoded . b64 decoded should equal original b16" $
      forM_ samples $ \(b16, _b32, b64) -> shouldBe (B16.encode . BSL.toStrict <$> B64.decode b64 ) (Right b16)

    it "b32 encoded . b16 decoded should equal original b32" $
      forM_ samples $ \(b16, b32, _b64) -> shouldBe (B32.encode
#if MIN_VERSION_base16_bytestring(1,0,0)
        <$> B16.decode b16) (Right b32)
#else
        $ fst $ B16.decode b16) (b32)

#endif

    it "b64 encoded . b16 decoded should equal original b64" $
      forM_ samples $ \(b16, _b32, b64) -> shouldBe (B64.encode . BSL.fromStrict
#if MIN_VERSION_base16_bytestring(1,0,0)
        <$> B16.decode b16) (Right b64)
#else
        $ fst $ B16.decode b16 ) (b64)
#endif

