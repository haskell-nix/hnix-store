module RealisationSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()

import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.StorePath qualified
import System.Nix.OutputName qualified
import System.Nix.Realisation qualified

spec :: Spec
spec = do
  describe "BuildTraceKey" $ do
    prop "roundtrips" $
      roundtrips
        ( Data.Text.Lazy.toStrict
        . Data.Text.Lazy.Builder.toLazyText
        . System.Nix.Realisation.buildTraceKeyBuilder
            (System.Nix.StorePath.unStorePathName . System.Nix.OutputName.unOutputName)
        )
        ( System.Nix.Realisation.buildTraceKeyParser
            System.Nix.OutputName.mkOutputName
        )
