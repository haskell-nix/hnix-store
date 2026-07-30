{-# LANGUAGE OverloadedStrings #-}

module NixSerializerSpec (spec) where

import Crypto.Hash (MD5, SHA1, SHA256, SHA512)
import Data.Set qualified
import Data.Some (Some)
import Data.Time (UTCTime)
import Test.Hspec (Expectation, Spec, describe, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildResult(..), BuildSuccess(..))
import System.Nix.Derivation.Traditional qualified
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.Logger ()
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion(..), ProtoFeature(..))
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))

-- | Test for roundtrip using @NixSerializer@
roundtripS
  :: forall e a
   . ( Eq a
     , Show a
     , Eq e
     , Show e
     )
  => NixSerializer e a
  -> a
  -> Expectation
roundtripS serializer a =
    runG serializer (runP serializer a)
    `shouldBe` Right a

spec :: Spec
spec = parallel $ do
  describe "Prim" $ do
    prop "Int" $ roundtripS @() @Int int
    prop "Bool" $ roundtripS bool
    prop "ByteString" $ roundtripS byteString
    prop "Text" $ roundtripS text
    prop "Maybe Text" $ roundtripS maybeText
    prop "UTCTime" $ roundtripS @() @UTCTime time

  describe "Combinators" $ do
    prop "list" $ roundtripS @() @[Int] (list int)
    prop "set" $ roundtripS (set byteString)
    prop "hashSet" $ roundtripS (hashSet byteString)
    prop "mapS" $ roundtripS (mapS (int @Int) byteString)

  describe "Complex" $ do
    prop "DSum HashAlgo Digest" $ roundtripS namedDigest

    prop "BuildResult"
      $ \sd pv ->
          let pv' = pv { protoVersion_features = Data.Set.singleton ProtoFeature_RealisationWithPathNotHash }
          in roundtripS (buildResult sd pv')
          . (\x -> x { buildResultStatus = case buildResultStatus x of
                        Right (BuildSuccess st _bo) -> Right (BuildSuccess st mempty)
                        Left f -> Left f
                     })
          . (\x -> x { buildResultCpuUser = Nothing
                     , buildResultCpuSystem = Nothing
                     }
            )

    prop "StorePath" $ \sd ->
      roundtripS (storePath sd)

    prop "StorePathHashPart" $
      roundtripS storePathHashPart

    prop "StorePathName" $
      roundtripS storePathName

    prop "Metadata (StorePath)" $ \sd ->
      roundtripS (pathMetadata sd)

    prop "Some HashAlgo" $
      roundtripS someHashAlgo

    describe "Digest" $ do
      prop "MD5" $ roundtripS . digest @MD5
      prop "SHA1" $ roundtripS . digest @SHA1
      prop "SHA256" $ roundtripS . digest @SHA256
      prop "SHA512" $ roundtripS . digest @SHA512

    prop "Derivation" $ \sd drv ->
      roundtripS (basicDerivation sd) $
        System.Nix.Derivation.Traditional.withoutName drv

    prop "SingleDerivedPath" $ \sd ->
      roundtripS (singleDerivedPath sd)

    prop "ProtoVersion" $ roundtripS @() @ProtoVersion protoVersion

    prop "ProtoFeature" $ roundtripS protoFeature
    prop "Set ProtoFeature" $ roundtripS protoFeatures

    describe "Logger" $ do
      prop "ActivityID" $ roundtripS activityID
      prop "Maybe Activity" $ roundtripS maybeActivity
      prop "ActivityResult" $ roundtripS activityResult
      prop "Field" $ roundtripS field
      prop "Trace" $ roundtripS trace
      prop "ErrorInfo" $ roundtripS errorInfo
      prop "LoggerOpCode" $ roundtripS loggerOpCode
      prop "Verbosity" $ roundtripS verbosity
      prop "Logger"
        $ forAll (arbitrary :: Gen ProtoVersion)
        $ \pv -> roundtripS (logger pv)

  describe "Handshake" $ do
    prop "WorkerMagic" $ roundtripS workerMagic
    prop "TrustedFlag" $ roundtripS trustedFlag

  describe "Worker protocol" $ do
    prop "WorkerOp" $ roundtripS workerOp
    prop "StoreText" $ roundtripS storeText

    prop "StoreRequest"
      $ \sd -> forAll arbitrary $ \pv ->
          forAll (arbitrary `suchThat` (restrictProtoVersion pv))
          $ roundtripS (storeRequest sd pv)

  describe "StoreReply" $ do
    prop "()" $ roundtripS opSuccess
    prop "GCResult" $ \sd -> roundtripS (gcResult sd)
    prop "GCRoot" $ roundtripS gcRoot
    prop "Missing" $ \sd -> roundtripS (missing sd)
    prop "Maybe (Metadata StorePath)" $ \sd -> roundtripS (maybePathMetadata sd)

restrictProtoVersion :: ProtoVersion -> Some StoreRequest -> Bool
restrictProtoVersion _ _ = True

