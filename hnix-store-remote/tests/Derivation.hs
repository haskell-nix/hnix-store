{-# LANGUAGE DataKinds           #-}

module Derivation where

import           Nix.Derivation                 ( Derivation(..)
                                                , DerivationOutput(..)
                                                )
import           System.Nix.StorePath           ( StorePath
                                                , storePathToText
                                                )

import           System.Nix.Store.Remote        ( MonadStore
                                                , addToStore
                                                , addTextToStore
                                                )
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified System.Nix.Derivation
import qualified System.Nix.StorePath
import qualified System.Directory
import           Crypto.Hash                    ( SHA256 )

drvSample :: StorePath -> StorePath -> StorePath -> Derivation StorePath Text
drvSample builder' buildScript out = Derivation
  { outputs = Data.Map.fromList [("out", DerivationOutput out "sha256" "test")]
  , inputDrvs = Data.Map.fromList [(builder', Data.Set.fromList ["out"])]
  , inputSrcs = Data.Set.fromList [buildScript]
  , platform  = "x86_64-linux"
  , builder   = storePathToText builder'
  , args      = Data.Vector.fromList ["-e", storePathToText buildScript]
  , env       = Data.Map.fromList [("testEnv", "true")]
  }

withBash :: (StorePath -> MonadStore a) -> MonadStore a
withBash action = do
  mfp <- liftIO $ System.Directory.findExecutable "bash"
  case mfp of
    Nothing -> error "No bash executable found"
    Just fp -> do
      let Right n = System.Nix.StorePath.makeStorePathName "bash"
      pth <- addToStore @SHA256 n fp False (pure True) False
      action pth

withBuildScript :: (StorePath -> MonadStore a) -> MonadStore a
withBuildScript action = do
  pth <- addTextToStore "buildScript"
                        (Data.Text.concat ["declare -xp", "export > $out"])
                        mempty
                        False

  action pth

withDerivation
  :: (StorePath -> Derivation StorePath Text -> MonadStore a) -> MonadStore a
withDerivation action = withBuildScript $ \buildScript -> withBash $ \bash ->
  do
    outputPath <- addTextToStore "wannabe-output" "" mempty False

    let d = drvSample bash buildScript outputPath

    pth <- addTextToStore
      "hnix-store-derivation"
      ( Data.Text.Lazy.toStrict
      $ Data.Text.Lazy.Builder.toLazyText
      $ System.Nix.Derivation.buildDerivation d
      )
      mempty
      False

    liftIO $ print d
    action pth d

