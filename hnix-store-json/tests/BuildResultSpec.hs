{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BuildResultSpec where

import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Paths_hnix_store_json (getDataFileName)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

import System.Nix.Build (BuildResult(..), BuildFailure(..), BuildFailureStatus(..))
import System.Nix.JSON ()

-- For success.json, we'll just check that it parses successfully
-- The exact BuildTraceKey values are derived from the hash in the JSON

-- We just check that the failures parse correctly since they don't have complex nested structures

spec :: Spec
spec = do
  describe "upstream Nix test data" $ do
    it "parses success.json" $ do
      path <- getDataFileName "upstream-libstore-data/build-result/success.json"
      json <- BSL.readFile path
      let result = eitherDecode json :: Either String BuildResult
      result `shouldSatisfy` \case
        Right (BuildResult (Right _) 3 _ _ (Just 500000000) (Just 604000000)) -> True
        _ -> False

    it "parses not-deterministic.json" $ do
      path <- getDataFileName "upstream-libstore-data/build-result/not-deterministic.json"
      json <- BSL.readFile path
      let result = eitherDecode json :: Either String BuildResult
      result `shouldSatisfy` \case
        Right (BuildResult (Left (BuildFailure BuildFailureStatus_NotDeterministic "no idea why" False)) 1 _ _ Nothing Nothing) -> True
        _ -> False

    it "parses output-rejected.json" $ do
      path <- getDataFileName "upstream-libstore-data/build-result/output-rejected.json"
      json <- BSL.readFile path
      let result = eitherDecode json :: Either String BuildResult
      result `shouldSatisfy` \case
        Right (BuildResult (Left (BuildFailure BuildFailureStatus_OutputRejected "no idea why" False)) 3 _ _ Nothing Nothing) -> True
        _ -> False
