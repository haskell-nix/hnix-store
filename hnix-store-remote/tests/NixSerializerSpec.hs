{-# LANGUAGE OverloadedStrings #-}

module NixSerializerSpec (spec) where

import Crypto.Hash (MD5, SHA1, SHA256, SHA512)
import Data.Some (Some(Some))
import Test.Hspec (Expectation, Spec, describe, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)

import Data.Time.Clock.POSIX qualified

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildResult(..), BuildSuccess(..), BuildFailure(..))
import System.Nix.Derivation.Traditional qualified
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.Logger (Logger(..))
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion(..))
import System.Nix.Store.Remote.Types.StoreConfig (ProtoStoreConfig(..))
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
    (runG serializer
     $ runP serializer a)
    `shouldBe` (pure a)

roundtripSReader
  :: forall r e a
  .  ( Eq a
     , Show a
     , Eq e
     , Show e
     )
  => (r -> NixSerializer e a)
  -> r
  -> a
  -> Expectation
roundtripSReader serializer r = roundtripS $ serializer r

spec :: Spec
spec = parallel $ do
  describe "Prim" $ do
    prop "Int" $ roundtripS @() $ int @Int
    prop "Bool" $ roundtripS bool
    prop "ByteString" $ roundtripS byteString
    prop "Text" $ roundtripS text
    prop "Maybe Text" $ roundtripS maybeText
    prop "UTCTime" $ roundtripS @() time

  describe "Combinators" $ do
    prop "list" $ roundtripS @() (list $ int @Int)
    prop "set" $ roundtripS (set byteString)
    prop "hashSet" $ roundtripS (hashSet byteString)
    prop "mapS" $ roundtripS (mapS (int @Int) byteString)

  describe "Complex" $ do
    prop "DSum HashAlgo Digest" $ roundtripS namedDigest

    describe "BuildResult" $ do
      prop "< 1.28"
        $ \sd -> forAll (arbitrary `suchThat` ((< 28) . protoVersion_minor))
        $ \pv ->
            roundtripS (buildResult sd pv)
            . (\x -> x { buildResultStatus = case buildResultStatus x of
                          Right (BuildSuccess st _bo) -> Right (BuildSuccess st mempty)
                          Left (BuildFailure st em _nd) -> Left (BuildFailure st em False)
                       })
            . (\x -> x { buildResultTimesBuilt = 0
                       , buildResultStartTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                       , buildResultStopTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                       , buildResultCpuUser = Nothing
                       , buildResultCpuSystem = Nothing
                       }
              )
      prop "= 1.28"
        $ \sd ->
            roundtripS (buildResult sd $ ProtoVersion 1 28)
            . (\x -> x { buildResultStatus = case buildResultStatus x of
                          Right s -> Right s
                          Left (BuildFailure st em _nd) -> Left (BuildFailure st em False)
                       })
            . (\x -> x { buildResultTimesBuilt = 0
                       , buildResultStartTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                       , buildResultStopTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                       , buildResultCpuUser = Nothing
                       , buildResultCpuSystem = Nothing
                       }
              )
      prop "> 1.28"
        $ \sd -> forAll (arbitrary `suchThat` ((> 28) . protoVersion_minor))
        $ \pv ->
            roundtripS (buildResult sd pv)
            . (\x -> x { buildResultCpuUser = Nothing
                       , buildResultCpuSystem = Nothing
                       }
              )

    prop "StorePath" $
      roundtripSReader storePath

    prop "StorePathHashPart" $
      roundtripS storePathHashPart

    prop "StorePathName" $
      roundtripS storePathName

    prop "Metadata (StorePath)" $
      roundtripSReader pathMetadata

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

    prop "ProtoVersion" $ roundtripS @() protoVersion

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
        $ roundtripS $ logger pv

  describe "Handshake" $ do
    prop "WorkerMagic" $ roundtripS workerMagic
    prop "TrustedFlag" $ roundtripS trustedFlag

  describe "Worker protocol" $ do
    prop "WorkerOp" $ roundtripS workerOp
    prop "StoreText" $ roundtripS storeText

    prop "StoreRequest"
      $ \testStoreConfig ->
          forAll (arbitrary `suchThat` (restrictProtoVersion (hasProtoVersion testStoreConfig)))
          $ roundtripS $ storeRequest
              (protoStoreConfigDir testStoreConfig)
              (protoStoreConfigProtoVersion testStoreConfig)

  describe "StoreReply" $ do
    prop "()" $ roundtripS opSuccess
    prop "GCResult" $ roundtripSReader gcResult
    prop "GCRoot" $ roundtripS gcRoot
    prop "Missing" $ roundtripSReader missing
    prop "Maybe (Metadata StorePath)" $ roundtripSReader maybePathMetadata

restrictProtoVersion :: ProtoVersion -> Some StoreRequest -> Bool
restrictProtoVersion v (Some (BuildPaths _ _)) | v < ProtoVersion 1 30 = False
restrictProtoVersion v (Some (QueryMissing _)) | v < ProtoVersion 1 30 = False
restrictProtoVersion _ _ = True

errorInfoIf :: Bool -> Logger -> Bool
errorInfoIf True  (Logger_Error (Right _)) = True
errorInfoIf False (Logger_Error (Left _))  = True
errorInfoIf _     (Logger_Error _)         = False
errorInfoIf _     _                        = True
