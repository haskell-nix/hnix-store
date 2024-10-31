{-# LANGUAGE OverloadedStrings #-}

module ReadOnlySpec where

import Data.Default.Class (Default(def))
import Test.Hspec (Spec, describe, it, shouldBe, pendingWith)

import Crypto.Hash (hash, Digest, SHA256(..))
import Data.ByteString (ByteString)
import System.Nix.StorePath (StorePath, StorePathName)
import System.Nix.ContentAddress (ContentAddressMethod(..))

import qualified Data.HashSet
import qualified System.Nix.StorePath

import System.Nix.Store.ReadOnly

testDigest :: Digest SHA256
testDigest = Crypto.Hash.hash @ByteString "testDigest"

testName :: StorePathName
testName =
    either undefined id
  $ System.Nix.StorePath.mkStorePathName "testFixed"

testPath :: StorePath
testPath =
    either undefined id
  $ System.Nix.StorePath.parsePathFromText
      def
      "/nix/store/9472ijanf79nlkb5n1yh57s7867p1930-testFixed"

testPath2 :: StorePath
testPath2 =
    either undefined id
  $ System.Nix.StorePath.parsePathFromText
      def
      "/nix/store/iali40m5597kikibjxw8cx1ssxlqnii3-testFixed"

spec :: Spec
spec = do
  describe "ReadOnly" $ do
    it "makeStorePath computes correct StorePath" $
      (pure
        $ makeStorePath
            def
            "test"
            testDigest
            testName
      )
      `shouldBe`
      System.Nix.StorePath.parsePathFromText
        def
        "/nix/store/iali40m5597kikibjxw8cx1ssxlqnii3-testFixed"

    describe "makeTextPath" $ do
      it "computes correct StorePath for empty refs" $
        (pure
          $ makeFixedOutputPath
              def
              ContentAddressMethod_Text
              testDigest
              mempty
              testName
        )
        `shouldBe`
        System.Nix.StorePath.parsePathFromText
          def
          "/nix/store/ync87sfmahhaqwnykzwbk31q96drm9vn-testFixed"

      it "computes correct StorePath for nonempty refs" $
        (pure
          $ makeFixedOutputPath
              def
              ContentAddressMethod_Text
              testDigest
              (Data.HashSet.fromList [ testPath, testPath2 ])
              testName
        )
        `shouldBe`
        System.Nix.StorePath.parsePathFromText
          def
          "/nix/store/jcvh84zapqndh8hva515d4y41s07n2g8-testFixed"

    describe "makeFixedOuputPath" $ do
      it "computes correct StorePath, recursive" $
        (pure
          $ makeFixedOutputPath
              def
              ContentAddressMethod_NixArchive
              testDigest
              mempty
              testName
        )
        `shouldBe`
        System.Nix.StorePath.parsePathFromText
          def
          "/nix/store/c0cgdqy9i3smyh3as8c4s6fg6nvwdpzy-testFixed"

      it "computes correct StorePath, non-recursive" $
        (pure
          $ makeFixedOutputPath
              def
              ContentAddressMethod_Flat
              testDigest
              mempty
              testName
        )
        `shouldBe`
        System.Nix.StorePath.parsePathFromText
          def
          "/nix/store/9472ijanf79nlkb5n1yh57s7867p1930-testFixed"

    it "computeStorePathForText computes correct StorePath" $
        (pure
          $ makeFixedOutputPath
              def
              ContentAddressMethod_Text
              (Crypto.Hash.hash ("test" :: ByteString) :: Digest SHA256)
              (Data.HashSet.fromList [ testPath ])
              testName
        )
        `shouldBe`
        System.Nix.StorePath.parsePathFromText
          def
          "/nix/store/838lq5qh5a88wsalcjpmj33bcnmpz3pc-testFixed"

    describe "computeStorePathForForPath" $ do
      it "computes correct StorePath" $
        pendingWith "needs IO and a sample directory to add"
