module Data.SerializerSpec (spec) where

import Control.Monad.Trans.Identity
import Data.Some
import Data.Serializer
import Data.Serializer.Example

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "Serializer" $ do
  prop "Roundtrips GADT protocol" $ \someCmd ->
    (runG cmdS $ runP cmdS someCmd)
    `shouldBe`
    (pure someCmd ::
      Either (GetSerializerError MyGetError)
        (Some Cmd))

  it "Handles getS error" $
    runG cmdSGetError (runPutS (cmdS @IdentityT) (Some (Cmd_Bool True)))
    `shouldBe`
    Left (SerializerError_Get MyGetError_Example)

  it "Handles getS fail" $
    runG cmdSGetFail (runPutS (cmdS @IdentityT) (Some (Cmd_Bool True)))
    `shouldBe`
    Left (SerializerError_GetFail @MyGetError "Failed reading: no parse\nEmpty call stack\n")

  prop "Roundtrips elaborate example" $ \someCmd readerBool ->
    (runGRest (cmdSRest readerBool) readerBool 0
     $ runPRest (cmdSRest readerBool) someCmd)
    `shouldBe`
    (pure someCmd ::
      Either (GetSerializerError MyGetError)
        (Some Cmd))
