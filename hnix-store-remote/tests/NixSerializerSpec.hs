{-# LANGUAGE OverloadedStrings #-}

module NixSerializerSpec (spec) where

import Crypto.Hash (MD5, SHA1, SHA256, SHA512)
import Data.Some (Some(Some))
import Data.Time (UTCTime)
import Test.Hspec (Expectation, Spec, describe, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)

import System.Nix.Arbitrary ()
import System.Nix.Derivation (Derivation(inputDrvs))
import System.Nix.Build (BuildResult(..))
import System.Nix.StorePath (StoreDir)
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.Logger (Logger(..))
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion(..))
import System.Nix.Store.Remote.Types.StoreConfig (TestStoreConfig(..))
import System.Nix.Store.Remote.Types.StoreRequest (StoreRequest(..))

-- | Test for roundtrip using @NixSerializer@
roundtripSReader
  :: forall r e a
   . ( Eq a
     , Show a
     , Eq e
     , Show e
     )
  => NixSerializer r e a
  -> r
  -> a
  -> Expectation
roundtripSReader serializer readerVal a =
    (runG serializer readerVal
    <$> runP serializer readerVal a)
    `shouldBe` (pure $ pure a)

roundtripS
  :: ( Eq a
     , Show a
     , Eq e
     , Show e
     )
  => NixSerializer () e a
  -> a
  -> Expectation
roundtripS serializer = roundtripSReader serializer ()

spec :: Spec
spec = parallel $ do
  describe "Prim" $ do
    prop "Int" $ roundtripS @Int @() int
    prop "Bool" $ roundtripS bool
    prop "ByteString" $ roundtripS byteString
    prop "Text" $ roundtripS text
    prop "Maybe Text" $ roundtripS maybeText
    prop "UTCTime" $ roundtripS @UTCTime @() time

  describe "Combinators" $ do
    prop "list" $ roundtripS @[Int] @() (list int)
    prop "set" $ roundtripS (set byteString)
    prop "hashSet" $ roundtripS (hashSet byteString)
    prop "mapS" $ roundtripS (mapS (int @Int) byteString)

  describe "Complex" $ do
    prop "DSum HashAlgo Digest" $ roundtripS namedDigest

    describe "BuildResult" $ do
      prop "< 1.28"
        $ \sd -> forAll (arbitrary `suchThat` ((< 28) . protoVersion_minor))
        $ \pv ->
            roundtripSReader @TestStoreConfig buildResult (TestStoreConfig sd pv)
            . (\x -> x { buildResultBuiltOutputs = Nothing })
            . (\x -> x { buildResultTimesBuilt = Nothing
                       , buildResultIsNonDeterministic = Nothing
                       , buildResultStartTime = Nothing
                       , buildResultStopTime = Nothing
                       }
              )
      prop "= 1.28"
        $ \sd ->
            roundtripSReader @TestStoreConfig buildResult (TestStoreConfig sd (ProtoVersion 1 28))
            . (\x -> x { buildResultTimesBuilt = Nothing
                       , buildResultIsNonDeterministic = Nothing
                       , buildResultStartTime = Nothing
                       , buildResultStopTime = Nothing
                       }
              )
      prop "> 1.28"
        $ \sd -> forAll (arbitrary `suchThat` ((> 28) . protoVersion_minor))
        $ \pv ->
            roundtripSReader @TestStoreConfig buildResult (TestStoreConfig sd pv)

    prop "StorePath" $
      roundtripSReader @StoreDir storePath

    prop "StorePathHashPart" $
      roundtripS storePathHashPart

    prop "StorePathName" $
      roundtripS storePathName

    prop "Metadata (StorePath)" $
      roundtripSReader @StoreDir pathMetadata

    prop "Some HashAlgo" $
      roundtripS someHashAlgo

    describe "Digest" $ do
      prop "MD5" $ roundtripS . digest @MD5
      prop "SHA1" $ roundtripS . digest @SHA1
      prop "SHA256" $ roundtripS . digest @SHA256
      prop "SHA512" $ roundtripS . digest @SHA512

    prop "Derivation" $ \sd ->
      roundtripSReader @StoreDir derivation sd
      . (\drv -> drv { inputDrvs = mempty })

    prop "ProtoVersion" $ roundtripS @ProtoVersion @() protoVersion

    describe "Logger" $ do
      prop "ActivityID" $ roundtripS activityID
      prop "Maybe Activity" $ roundtripS maybeActivity
      prop "ActivityResult" $ roundtripS activityResult
      prop "Field" $ roundtripS field
      prop "Trace" $ roundtripS trace
      prop "BasicError" $ roundtripS basicError
      prop "ErrorInfo" $ roundtripS errorInfo
      prop "LoggerOpCode" $ roundtripS loggerOpCode
      prop "Verbosity" $ roundtripS verbosity
      prop "Logger"
        $ forAll (arbitrary :: Gen ProtoVersion)
        $ \pv ->
            forAll (arbitrary `suchThat` errorInfoIf (protoVersion_minor pv >= 26))
        $ roundtripSReader logger pv

  describe "Handshake" $ do
    prop "WorkerMagic" $ roundtripS workerMagic
    prop "TrustedFlag" $ roundtripS trustedFlag

  describe "Worker protocol" $ do
    prop "WorkerOp" $ roundtripS workerOp
    prop "StoreText" $ roundtripS storeText

    prop "StoreRequest"
      $ \testStoreConfig ->
          forAll (arbitrary `suchThat` (restrictProtoVersion (hasProtoVersion testStoreConfig)))
          $ roundtripSReader @TestStoreConfig storeRequest testStoreConfig

  describe "StoreReply" $ do
    prop "GCResult" $ roundtripSReader @StoreDir gcResult
    prop "Missing" $ roundtripSReader @StoreDir missing
    prop "Maybe (Metadata StorePath)" $ roundtripSReader @StoreDir maybePathMetadata

restrictProtoVersion :: ProtoVersion -> Some StoreRequest -> Bool
restrictProtoVersion v (Some (BuildPaths _ _)) | v < ProtoVersion 1 30 = False
restrictProtoVersion _ (Some (BuildDerivation _ drv _)) = inputDrvs drv == mempty
restrictProtoVersion v (Some (QueryMissing _)) | v < ProtoVersion 1 30 = False
restrictProtoVersion _ _ = True

errorInfoIf :: Bool -> Logger -> Bool
errorInfoIf True  (Logger_Error (Right _)) = True
errorInfoIf False (Logger_Error (Left _))  = True
errorInfoIf _     (Logger_Error _)         = False
errorInfoIf _     _                        = True
