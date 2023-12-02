{-# LANGUAGE OverloadedStrings #-}

module NixSerializerSpec (spec) where

import Crypto.Hash (MD5, SHA1, SHA256, SHA512)
import Data.Dependent.Sum (DSum((:=>)))
import Data.Time (UTCTime)
import Data.Word (Word64)
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)

import qualified System.Nix.Hash

import System.Nix.Arbitrary ()
import System.Nix.Derivation (Derivation(inputDrvs))
import System.Nix.StorePath (StoreDir)
import System.Nix.StorePath.Metadata (Metadata(..))
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.Logger (ErrorInfo(..), Logger(..),  Trace(..))
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion(..), ProtoVersion(..))
import System.Nix.Store.Remote.Types.StoreConfig (TestStoreConfig)
import System.Nix.Store.Remote.Types.WorkerOp (WorkerOp(..))

-- WIP
import Data.Some (Some(Some))
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
    prop "Maybe Text"
      $ forAll (arbitrary `suchThat` (/= Just ""))
      $ roundtripS maybeText
    prop "UTCTime" $ roundtripS @UTCTime @() time

  describe "Combinators" $ do
    prop "list" $ roundtripS @[Int] @() (list int)
    prop "set" $ roundtripS (set byteString)
    prop "hashSet" $ roundtripS (hashSet byteString)
    prop "mapS" $ roundtripS (mapS (int @Int) byteString)

  describe "Complex" $ do
    prop "BuildResult" $ roundtripS buildResult

    prop "StorePath" $
      roundtripSReader @StoreDir storePath

    prop "StorePathHashPart" $
      roundtripS storePathHashPart

    prop "StorePathName" $
      roundtripS storePathName

    let narHashIsSHA256 Metadata{..} =
          case narHash of
            (System.Nix.Hash.HashAlgo_SHA256 :=> _) -> True
            _ -> False

    prop "Metadata (StorePath)"
      $ \sd -> forAll (arbitrary `suchThat` (\m -> narHashIsSHA256 m && narBytes m /= Just 0))
      $ roundtripSReader @StoreDir pathMetadata sd

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
      prop "Trace"
        $ forAll (arbitrary `suchThat` ((/= Just 0) . tracePosition))
        $ roundtripS trace
      prop "BasicError" $ roundtripS basicError
      prop "ErrorInfo"
        $ forAll (arbitrary
                  `suchThat`
                    (\ErrorInfo{..}
                        -> errorInfoPosition /= Just 0
                           && all ((/= Just 0) . tracePosition) errorInfoTraces
                    )
                 )
        $ roundtripS errorInfo
      prop "LoggerOpCode" $ roundtripS loggerOpCode
      prop "Verbosity" $ roundtripS verbosity
      prop "Logger"
        $ forAll (arbitrary :: Gen ProtoVersion)
        $ \pv ->
            forAll (arbitrary `suchThat` errorInfoIf (protoVersion_minor pv >= 26))
        $ roundtripSReader logger pv

  describe "Enums" $ do
    let it' name constr value =
          it name
            $ (runP enum () constr)
            `shouldBe`
            (runP (int @Word64) () value)

    describe "WorkerOp enum order matches Nix" $ do
      it' "IsValidPath"           WorkerOp_IsValidPath            1
      it' "BuildPathsWithResults" WorkerOp_BuildPathsWithResults 46

  describe "Worker protocol" $ do
    prop "StoreText" $ roundtripS storeText

    prop "StoreRequest"
      $ \testStoreConfig ->
          forAll (arbitrary `suchThat` (hacks (hasProtoVersion testStoreConfig)))
          $ roundtripSReader @TestStoreConfig storeRequest testStoreConfig

hacks :: ProtoVersion -> Some StoreRequest -> Bool
hacks v (Some (BuildPaths _ _)) | v < ProtoVersion 1 30 = False
hacks _ (Some (BuildDerivation _ drv _)) = inputDrvs drv == mempty
hacks v (Some (QueryMissing _)) | v < ProtoVersion 1 30 = False
hacks _ _ = True

errorInfoIf :: Bool -> Logger -> Bool
errorInfoIf True (Logger_Error (Right x)) = noJust0s x
  where
    noJust0s :: ErrorInfo -> Bool
    noJust0s ErrorInfo{..} =
      errorInfoPosition /= Just 0
      && all ((/= Just 0) . tracePosition) errorInfoTraces
errorInfoIf False (Logger_Error (Left _)) = True
errorInfoIf _ (Logger_Error _) = False
errorInfoIf _ _ = True
