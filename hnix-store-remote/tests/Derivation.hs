{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}

module Derivation where

import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)
import           Nix.Derivation (Derivation(..), DerivationOutput(..))
import           System.Nix.StorePath (StorePath, storePathToText)

import           System.Nix.Store.Remote (addToStore, addTextToStore)
import           System.Nix.Hash (HashAlgorithm(Truncated, SHA256))

import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Nix.Derivation
import qualified System.Nix.Derivation
import qualified System.Nix.StorePath
import qualified System.Which

drvSample :: StorePath -> StorePath -> StorePath -> Derivation StorePath Text
drvSample builder buildScript out = Derivation {
    outputs   = Data.Map.fromList [ ("out", DerivationOutput out "sha256" "test") ]
  , inputDrvs = Data.Map.fromList [ (builder, Data.Set.fromList [ "out" ]) ]
  , inputSrcs = Data.Set.fromList [ buildScript ]
  , platform  = "x86_64-linux"
  , builder   = storePathToText builder
  , args      = Data.Vector.fromList ["-e", storePathToText buildScript ]
  , env       = Data.Map.fromList [("testEnv", "true")]
  }

withBash action = do
  mfp <- liftIO $ System.Which.which "bash"
  case mfp of
    Nothing -> error "No bash executable found"
    Just fp -> do
      let Right n = System.Nix.StorePath.makeStorePathName "bash"
      path <- addToStore @SHA256 n fp False (\p pt -> pure True) False
      action path

withBuildScript action = do
  path <- addTextToStore
    "buildScript"
    (Data.Text.concat [ "declare -xp", "export > $out" ])
    mempty
    False

  action path

withDerivation action = withBuildScript $ \buildScript -> withBash $ \bash -> do
  outputPath <- addTextToStore "wannabe-output" "" mempty False

  let d = drvSample bash buildScript outputPath

  path <- addTextToStore
    "hnix-store-derivation"
    (Data.Text.Lazy.toStrict
      $ Data.Text.Lazy.Builder.toLazyText
      $ System.Nix.Derivation.buildDerivation d
    )
    mempty
    False

  liftIO $ print d
  action path d

