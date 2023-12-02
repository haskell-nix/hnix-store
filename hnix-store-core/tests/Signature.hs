{-# LANGUAGE OverloadedStrings #-}
-- Inspired by https://cl.tvl.fyi/c/depot/+/10081/1/tvix/nix-compat/src/narinfo/signature.rs
-- and https://github.com/nix-community/go-nix/pull/93
-- by @flokli and @zimbatm

module Signature where

import qualified Data.ByteString as BS
import Test.Hspec
import Data.Text (Text)
import qualified Crypto.PubKey.Ed25519
import qualified System.Nix.Base
import System.Nix.Base (BaseEncoding(Base64))
import Crypto.Error (CryptoFailable(..))

import System.Nix.Signature

spec_signature :: Spec
spec_signature = do

  describe "signature parser" $ do

    it "parses names" $ do
      shouldParseName "cache.nixos.org-1:TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ==" "cache.nixos.org-1"

    it "fails on invalid signatures" $ do
      shouldNotParse ""
      shouldNotParse "asdf"
      shouldNotParse "cache.nixos.org-1:"
      shouldNotParse ":TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ=="
      shouldNotParse "cache.nixos.org-1TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ=="
      shouldNotParse "cache.nixos.org-1:sTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ=="

    it "parses verifying signatures" $ do
      shouldVerify "cache.nixos.org-1:TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ==" pubkeyNixosOrg fingerprint
      shouldVerify "cache.nixos.org-2:TsTTb3WGTZKphvYdBHXwo6weVILmTytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ==" pubkeyNixosOrg fingerprint

    it "parses non-verifying signatures" $ do
      shouldNotVerify "cache.nixos.org-1:TsTTb000000000000000000000000ytUjLB+vcX89fOjjRicCHmKA4RCPMVLkj6TMJ4GMX3HPVWRdD1hkeKZBQ==" pubkeyNixosOrg fingerprint

fingerprint :: BS.ByteString
fingerprint = "1;/nix/store/syd87l2rxw8cbsxmxl853h0r6pdwhwjr-curl-7.82.0-bin;sha256:1b4sb93wp679q4zx9k1ignby1yna3z7c4c2ri3wphylbc2dwsys0;196040;/nix/store/0jqd0rlxzra1rs38rdxl43yh6rxchgc6-curl-7.82.0,/nix/store/6w8g7njm4mck5dmjxws0z1xnrxvl81xa-glibc-2.34-115,/nix/store/j5jxw3iy7bbz4a57fh9g2xm2gxmyal8h-zlib-1.2.12,/nix/store/yxvjs9drzsphm9pcf42a4byzj1kb9m7k-openssl-1.1.1n";

forceDecodeB64Pubkey :: Text -> Crypto.PubKey.Ed25519.PublicKey
forceDecodeB64Pubkey b64EncodedPubkey = let
  decoded = case System.Nix.Base.decodeWith Base64 b64EncodedPubkey of
    Left err -> error err
    Right x -> x
  in case Crypto.PubKey.Ed25519.publicKey decoded of
    CryptoFailed err -> (error . show) err
    CryptoPassed x -> x

pubkeyNixosOrg :: Crypto.PubKey.Ed25519.PublicKey
pubkeyNixosOrg = forceDecodeB64Pubkey "6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

shouldNotParse :: Text -> Expectation
shouldNotParse encoded = case parseNarSignature encoded of
  Left _ -> pure ()
  Right _ -> expectationFailure "should not have parsed"

shouldParseName :: Text -> Text -> Expectation
shouldParseName encoded name = case parseNarSignature encoded of
  Left err -> expectationFailure err
  Right narSig -> shouldBe name (publicKey narSig)

shouldVerify :: Text -> Crypto.PubKey.Ed25519.PublicKey -> BS.ByteString -> Expectation
shouldVerify encoded pubkey msg = case parseNarSignature encoded of
  Left err -> expectationFailure err
  Right narSig -> let
    (Signature sig') = sig narSig
    in sig' `shouldSatisfy` Crypto.PubKey.Ed25519.verify pubkey msg

shouldNotVerify :: Text -> Crypto.PubKey.Ed25519.PublicKey -> BS.ByteString -> Expectation
shouldNotVerify encoded pubkey msg = case parseNarSignature encoded of
  Left err -> expectationFailure err
  Right narSig -> let
    (Signature sig') = sig narSig
    in sig' `shouldNotSatisfy` Crypto.PubKey.Ed25519.verify pubkey msg
