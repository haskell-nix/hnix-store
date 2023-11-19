{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SerializeSpec where

import Data.ByteString (ByteString)
import Data.Fixed (Uni)
import Data.HashSet (HashSet)
import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, runPut)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Either
import qualified Data.HashSet
import qualified Data.Time.Clock.POSIX
import qualified System.Nix.Build

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildMode, BuildStatus)
import System.Nix.Derivation (Derivation(..))
import System.Nix.StorePath (StoreDir, StorePath)
import System.Nix.Store.Remote.Serialize (getDerivation, putDerivation)
import System.Nix.Store.Remote.Serialize.Prim

roundTrip :: (Eq a, Show a) => Putter a -> Get a -> a -> Property
roundTrip p g a = res === Right a
  where res = runGet g (runPut (p a))

-- * Prim
-- ** Int

prop_int :: Int -> Property
prop_int = roundTrip putInt getInt

-- ** Bool

prop_bool :: Bool -> Property
prop_bool = roundTrip putBool getBool

-- ** UTCTime

prop_time :: Int -> Property
prop_time =
 roundTrip
  (putTime . Data.Time.Clock.POSIX.posixSecondsToUTCTime . toSeconds)
  (fromSeconds . Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds <$> getTime)
  where
    -- scale to seconds and back
    toSeconds :: Int -> NominalDiffTime
    toSeconds n = realToFrac (toEnum n :: Uni)
    fromSeconds :: NominalDiffTime -> Int
    fromSeconds = (fromEnum :: Uni -> Int) . realToFrac

-- ** Combinators

prop_many :: [Int] -> Property
prop_many = roundTrip (putMany putInt) (getMany getInt)

-- ** ByteString

prop_bytestring :: ByteString -> Property
prop_bytestring = roundTrip putByteString getByteString

prop_bytestrings :: [ByteString] -> Property
prop_bytestrings = roundTrip putByteStrings getByteStrings

-- ** Text

prop_text :: Text -> Property
prop_text = roundTrip putText getText

prop_texts :: [Text] -> Property
prop_texts = roundTrip putTexts getTexts

-- ** StorePath

prop_path :: StoreDir -> StorePath -> Property
prop_path = \sd ->
  roundTrip
    (putPath sd)
    (Data.Either.fromRight undefined <$> getPath sd)

prop_paths :: StoreDir -> HashSet StorePath -> Property
prop_paths = \sd ->
  roundTrip
    (putPaths sd)
    (Data.HashSet.map (Data.Either.fromRight undefined) <$> getPaths sd)

-- * Serialize
roundTripS :: (Eq a, Serialize a, Show a) => a -> Property
roundTripS a = res === Right a
  where res = runGet get (runPut (put a))

-- ** Text

prop_Text :: Text -> Property
prop_Text = roundTripS

-- ** BuildMode

prop_buildMode :: BuildMode -> Property
prop_buildMode = roundTripS

-- ** BuildStatus

prop_buildStatus :: BuildStatus -> Property
prop_buildStatus = roundTripS

-- ** BuildResult

prop_buildResult :: Property
prop_buildResult =
  forAll (arbitrary `suchThat` ((/= Just "") . System.Nix.Build.errorMessage))
  $ \br ->
      roundTripS
        $ br { System.Nix.Build.startTime = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
             , System.Nix.Build.stopTime  = Data.Time.Clock.POSIX.posixSecondsToUTCTime 0
             }

-- ** Enums

spec_buildEnums :: Spec
spec_buildEnums =
  let it' name constr value = it name $ runPut (put constr) `shouldBe` runPut (putInt value)
  in do
    describe "Build enum order matches Nix" $ do
      it' "Normal" System.Nix.Build.Normal 0
      it' "Repair" System.Nix.Build.Repair 1
      it' "Check"  System.Nix.Build.Check  2

    describe "BuildStatus enum order matches Nix" $ do
      it' "Built"                  System.Nix.Build.Built                   0
      it' "Substituted"            System.Nix.Build.Substituted             1
      it' "AlreadyValid"           System.Nix.Build.AlreadyValid            2
      it' "PermanentFailure"       System.Nix.Build.PermanentFailure        3
      it' "InputRejected"          System.Nix.Build.InputRejected           4
      it' "OutputRejected"         System.Nix.Build.OutputRejected          5
      it' "TransientFailure"       System.Nix.Build.TransientFailure        6
      it' "CachedFailure"          System.Nix.Build.CachedFailure           7
      it' "TimedOut"               System.Nix.Build.TimedOut                8
      it' "MiscFailure"            System.Nix.Build.MiscFailure             9
      it' "DependencyFailed"       System.Nix.Build.DependencyFailed       10
      it' "LogLimitExceeded"       System.Nix.Build.LogLimitExceeded       11
      it' "NotDeterministic"       System.Nix.Build.NotDeterministic       12
      it' "ResolvesToAlreadyValid" System.Nix.Build.ResolvesToAlreadyValid 13
      it' "NoSubstituters"         System.Nix.Build.NoSubstituters         14

-- ** Derivation

prop_derivation :: StoreDir -> Derivation StorePath Text -> Property
prop_derivation sd drv =
  roundTrip
    (putDerivation sd)
    (getDerivation sd)
    -- inputDrvs is not used in remote protocol serialization
    (drv { inputDrvs = mempty })
