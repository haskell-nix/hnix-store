{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}
{-# language CPP #-}

module Hash where

import qualified Data.ByteString.Base16      as B16
import qualified System.Nix.Base32           as B32
import qualified Data.ByteString.Base64.Lazy as B64

import           Test.Hspec
import           Test.Tasty.QuickCheck

import           System.Nix.Hash
import           System.Nix.StorePath
import           System.Nix.Internal.Base
import           Crypto.Hash                ( MD5
                                            , SHA1
                                            , SHA256
                                            , hash
                                            , Digest
                                            )

spec_hash :: Spec
spec_hash = do

  describe "hashing parity with nix-store" $ do

    cmp "produces (base32 . sha256) of \"nix-output:foo\" the same as Nix does at the moment for placeholder \"foo\""
      NixBase32 (hash @ByteString @SHA256) "nix-output:foo" "1x0ymrsy7yr7i9wdsqy9khmzc1yy7nvxw6rdp72yzn50285s67j5"
    cmp "produces (base16 . md5) of \"Hello World\" the same as the thesis"
      Base16 (hash @ByteString @MD5) "Hello World" "b10a8db164e0754105b7a99be72e3fe5"
    cmp "produces (base32 . sha1) of \"Hello World\" the same as the thesis"
      NixBase32 (hash @ByteString @SHA1) "Hello World" "s23c9fs0v32pf6bhmcph5rbqsyl5ak8a"

    -- The example in question:
    -- https://nixos.org/nixos/nix-pills/nix-store-paths.html
    it "produces same base32 as nix pill flat file example" $ do
      shouldBe (encodeWith NixBase32 $ unStorePathHashPart $ mkStorePathHashPart @SHA256 "source:sha256:2bfef67de873c54551d884fdab3055d84d573e654efa79db3c0d7b98883f9ee3:/nix/store:myfile")
        "xv2iccirbrvklck36f1g7vldn5v58vck"
 where
  cmp :: String -> BaseEncoding -> (ByteString -> Digest a) -> ByteString -> Text -> SpecWith ()
  cmp t b f s h =
    it t $
      shouldBe (encodeDigestWith b $ f s) h

-- | Test that Nix-like base32 encoding roundtrips
prop_nixBase32Roundtrip :: Property
prop_nixBase32Roundtrip = forAllShrink nonEmptyString genericShrink $
  \x -> pure (encodeUtf8 x) === (B32.decode . B32.encode . encodeUtf8 $ x)
  where
    nonEmptyString :: Gen String
    nonEmptyString = listOf1 genSafeChar

    genSafeChar :: Gen Char
    genSafeChar = choose ('\1', '\127') -- ASCII without \NUL

-- | API variants
prop_nixBase16Roundtrip :: StorePathHashPart -> Property
prop_nixBase16Roundtrip x =
  pure (unStorePathHashPart x) === decodeWith Base16 (encodeWith Base16 $ unStorePathHashPart x)

-- | Hash encoding conversion ground-truth.
-- Similiar to nix/tests/hash.sh
spec_nixhash :: Spec
spec_nixhash = do

  describe "hashing parity with nix-nash" $ do

    cmp
      "b16 encoded . b32 decoded should equal original b16"
      B16.encode B32.decode b32s b16s

    cmp
      "b64 encoded . b32 decoded should equal original b64"
      (B64.encode . fromStrict) B32.decode b32s b64s

    cmp
      "b32 encoded . b64 decoded should equal original b32"
      (B32.encode . toStrict) B64.decode b64s b32s

    cmp
      "b16 encoded . b64 decoded should equal original b16"
      (B16.encode . toStrict) B64.decode b64s b16s

#if MIN_VERSION_base16_bytestring(1,0,0)
    cmp
      "b32 encoded . b16 decoded should equal original b32"
      B32.encode B16.decode b16s b32s

    cmp
      "b64 encoded . b16 decoded should equal original b64"
      (B64.encode . fromStrict) B16.decode b16s b64s
#else
    it "b32 encoded . b16 decoded should equal original b32" $
      traverse_ (\ b -> shouldBe (B32.encode $ fst $ B16.decode $ fst b) (snd b)) $ zip b16s b32s

    it "b64 encoded . b16 decoded should equal original b64" $
      traverse_ (\ b -> shouldBe (B64.encode . fromStrict $ fst $ B16.decode $ fst b) (snd b)) $ zip b16s b64s
#endif

 where
  cmp
    :: ( Eq b
       , Show b
       )
    => String
    -> (a -> b)
    -> (c -> Either String a)
    -> [c]
    -> [b]
    -> SpecWith ()
  cmp s f1 f2 b1 b2 = it s $ traverse_ (uncurry shouldBe . bimap (fmap f1 . f2) pure) $ zip b1 b2

  b16s = takeAxis (\(a,_,_) -> a)
  b32s = takeAxis (\(_,b,_) -> b)
  b64s = takeAxis (\(_,_,c) -> c)

  takeAxis f = fmap f samples

  samples =
    [ ( "800d59cfcd3c05e900cb4e214be48f6b886a08df"
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
