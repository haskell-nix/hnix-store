{-# LANGUAGE OverloadedStrings #-}

module SerializeSpec (spec) where

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Putter, runPut)
import Data.Text (Text)
import Test.Hspec (Expectation, Spec, describe, parallel)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import qualified Data.Either
import qualified Data.HashSet

import System.Nix.Arbitrary ()
import System.Nix.Build (BuildMode(..), BuildStatus(..))
import System.Nix.Derivation (Derivation(inputDrvs))
import System.Nix.Store.Remote.Arbitrary ()
import System.Nix.Store.Remote.Serialize (getDerivation, putDerivation)
import System.Nix.Store.Remote.Serialize.Prim
import System.Nix.Store.Remote.Types

-- | Test for roundtrip using @Putter@ and @Get@ functions
roundtrips2
  :: ( Eq a
     , Show a
     )
  => Putter a
  -> Get a
  -> a
  -> Expectation
roundtrips2 putter getter =
  roundtrips
    (runPut . putter)
    (runGet getter)

-- | Test for roundtrip using @Serialize@ instance
roundtripS
  :: ( Eq a
     , Serialize a
     , Show a
     )
  => a
  -> Expectation
roundtripS =
  roundtrips
    (runPut . put)
    (runGet get)

spec :: Spec
spec = parallel $ do
  describe "Prim" $ do
    prop "Int" $ roundtrips2 putInt (getInt @Int)
    prop "Bool" $ roundtrips2 putBool getBool
    prop "ByteString" $ roundtrips2 putByteString getByteString

  describe "Combinators" $ do
    prop "Many" $ roundtrips2 (putMany putInt) (getMany (getInt @Int))
    prop "[ByteString]" $ roundtrips2 putByteStrings getByteStrings
    prop "Text" $ roundtrips2 putText getText
    prop "[Text]" $ roundtrips2 putTexts getTexts

    prop "StorePath" $ \sd ->
      roundtrips2
        (putPath sd)
        (Data.Either.fromRight undefined <$> getPath sd)

    prop "HashSet StorePath" $ \sd ->
      roundtrips2
        (putPaths sd)
        (Data.HashSet.map (Data.Either.fromRight undefined) <$> getPaths sd)

  describe "Serialize instances" $ do
    prop "Text" $ roundtripS @Text
    prop "BuildMode" $ roundtripS @BuildMode
    prop "BuildStatus" $ roundtripS @BuildStatus

    prop "ProtoVersion" $ roundtripS @ProtoVersion

    prop "Derivation StorePath Text" $ \sd ->
      roundtrips2
        (putDerivation sd)
        (getDerivation sd)
        -- inputDrvs is not used in remote protocol serialization
        . (\drv -> drv { inputDrvs = mempty })

    describe "Logger" $ do
      prop "Activity" $ roundtripS @Activity
      prop "ActivityID" $ roundtripS @ActivityID
      prop "Activity" $ roundtripS @Activity
      prop "Field" $ roundtripS @Field
      prop "Trace" $ roundtripS @Trace
      prop "BasicError" $ roundtripS @BasicError
      prop "ErrorInfo" $ roundtripS @ErrorInfo
      prop "LoggerOpCode" $ roundtripS @LoggerOpCode
      prop "Verbosity" $ roundtripS @Verbosity
