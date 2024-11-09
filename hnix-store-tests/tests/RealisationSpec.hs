module RealisationSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified System.Nix.StorePath
import qualified System.Nix.OutputName
import qualified System.Nix.Realisation

spec :: Spec
spec = do
  describe "DerivationOutput" $ do
    prop "roundtrips" $
      roundtrips
        ( Data.Text.Lazy.toStrict
        . Data.Text.Lazy.Builder.toLazyText
        . System.Nix.Realisation.derivationOutputBuilder
            (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)
        )
        ( System.Nix.Realisation.derivationOutputParser
            System.Nix.OutputName.mkOutputName
        )
