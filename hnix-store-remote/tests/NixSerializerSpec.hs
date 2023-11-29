{-# LANGUAGE OverloadedStrings #-}

module NixSerializerSpec (spec) where

import Data.Fixed (Uni)
import Data.Time (NominalDiffTime)
import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, suchThat)
import Test.QuickCheck.Instances ()

import qualified Data.Time.Clock.POSIX
import qualified Data.Serializer
import qualified System.Nix.Build

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildResult)
import System.Nix.Derivation (Derivation(inputDrvs))
import System.Nix.StorePath (StoreDir, StorePath)
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types (ErrorInfo(..), Logger(..), ProtoVersion(..), Trace(..))

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
    prop "UTCTime" $ do
      let
        -- scale to seconds and back
        toSeconds :: Int -> NominalDiffTime
        toSeconds n = realToFrac (toEnum n :: Uni)
        fromSeconds :: NominalDiffTime -> Int
        fromSeconds = (fromEnum :: Uni -> Int) . realToFrac

      roundtripS @Int @() $
        Data.Serializer.mapIsoSerializer
          (fromSeconds . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds)
          (Data.Time.Clock.POSIX.posixSecondsToUTCTime . toSeconds)
          time

  describe "Combinators" $ do
    prop "list" $ roundtripS @[Int] @() (list int)
    prop "set" $ roundtripS (set byteString)
    prop "hashSet" $ roundtripS (hashSet byteString)
    prop "mapS" $ roundtripS (mapS (int @Int) byteString)

  describe "Complex" $ do
    prop "BuildResult"
      $ forAll (arbitrary `suchThat` ((/= Just "") . System.Nix.Build.errorMessage))
      $ \br ->
          roundtripS @BuildResult @() buildResult
            -- fix time to 0 as we test UTCTime above
            $ br { System.Nix.Build.startTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                 , System.Nix.Build.stopTime  = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
                 }

    prop "StorePath" $ \sd ->
      roundtripSReader @StoreDir path sd

    prop "Derivation" $ \sd ->
      roundtripS @(Derivation StorePath Text) @() (derivation sd)
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
        where
          errorInfoIf True (Logger_Error (Right x)) = noJust0s x
          errorInfoIf False (Logger_Error (Left _)) = True
          errorInfoIf _ (Logger_Error _) = False
          errorInfoIf _ _ = True
          noJust0s ErrorInfo{..} =
            errorInfoPosition /= Just 0
            && all ((/= Just 0) . tracePosition) errorInfoTraces
