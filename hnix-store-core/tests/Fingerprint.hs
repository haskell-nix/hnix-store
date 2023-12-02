{-# LANGUAGE OverloadedStrings #-}
-- Test case from https://code.tvl.fyi/commit/tvix/nix-compat/src/narinfo/fingerprint.rs?id=a834966efd64c1b2306241c3ef20f4258f6b9c4e

module Fingerprint where

import Crypto.Error (CryptoFailable(..))
import Data.Default.Class
import System.Nix.Base (decodeWith, BaseEncoding(Base64))
import System.Nix.Fingerprint
import System.Nix.Signature
import System.Nix.StorePath
import System.Nix.StorePath.Metadata
import System.Nix.Hash (mkNamedDigest)
import Data.Text (Text)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Test.Hspec

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Text

spec_fingerprint :: Spec
spec_fingerprint = do

  describe "fingerprint" $ do

    it "is valid for example metadata" $
      metadataFingerprint def exampleStorePath exampleMetadata `shouldBe` exampleFingerprint

    it "allows a successful signature verification" $ do
      let msg = Text.encodeUtf8 $ metadataFingerprint def exampleStorePath exampleMetadata
          Signature sig' = head $ sig <$> filter (\(NarSignature publicKey _) -> publicKey == "cache.nixos.org-1") (Set.toList (sigs exampleMetadata))
      sig' `shouldSatisfy` Ed25519.verify pubkey msg

exampleFingerprint :: Text
exampleFingerprint = "1;/nix/store/syd87l2rxw8cbsxmxl853h0r6pdwhwjr-curl-7.82.0-bin;sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0;196040;/nix/store/0jqd0rlxzra1rs38rdxl43yh6rxchgc6-curl-7.82.0,/nix/store/6w8g7njm4mck5dmjxws0z1xnrxvl81xa-glibc-2.34-115,/nix/store/j5jxw3iy7bbz4a57fh9g2xm2gxmyal8h-zlib-1.2.12,/nix/store/yxvjs9drzsphm9pcf42a4byzj1kb9m7k-openssl-1.1.1n";

exampleStorePath :: StorePath
exampleStorePath = forceRight $ parsePath def "/nix/store/syd87l2rxw8cbsxmxl853h0r6pdwhwjr-curl-7.82.0-bin"

exampleMetadata :: Metadata StorePath
exampleMetadata = Metadata
  { deriverPath = Just $ forceRight $ parsePath def "/nix/store/5rwxzi7pal3qhpsyfc16gzkh939q1np6-curl-7.82.0.drv"
  , narHash = forceRight $ mkNamedDigest "sha256" "1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0"
  , references = HashSet.fromList $ forceRight . parsePath def <$> ["/nix/store/0jqd0rlxzra1rs38rdxl43yh6rxchgc6-curl-7.82.0","/nix/store/6w8g7njm4mck5dmjxws0z1xnrxvl81xa-glibc-2.34-115","/nix/store/j5jxw3iy7bbz4a57fh9g2xm2gxmyal8h-zlib-1.2.12","/nix/store/yxvjs9drzsphm9pcf42a4byzj1kb9m7k-openssl-1.1.1n"]
  , registrationTime = UTCTime (fromOrdinalDate 0 0) 0
  , narBytes = Just 196040
  , trust = BuiltElsewhere
  , sigs = Set.fromList $ forceRight . parseNarSignature <$> ["cache.nixos.org-1:TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ==", "test1:519iiVLx/c4Rdt5DNt6Y2Jm6hcWE9+XY69ygiWSZCNGVcmOcyL64uVAJ3cV8vaTusIZdbTnYo9Y7vDNeTmmMBQ=="]
  , contentAddress = Nothing
  }

pubkey :: Ed25519.PublicKey
pubkey = forceDecodeB64Pubkey "6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

forceDecodeB64Pubkey :: Text -> Ed25519.PublicKey
forceDecodeB64Pubkey b64EncodedPubkey = let
  decoded = forceRight $ decodeWith Base64 b64EncodedPubkey
  in case Ed25519.publicKey decoded of
    CryptoFailed err -> (error . show) err
    CryptoPassed x -> x

forceRight :: Either a b -> b
forceRight = \case
  Right x -> x
  _ -> error "fromRight failed"

