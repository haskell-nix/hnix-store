
module Derivation where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Golden (goldenVsFile)

import           System.Nix.Derivation (parseDerivation, buildDerivation)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

processDerivation source dest = do
  contents <- Data.Text.IO.readFile source
  case Data.Attoparsec.Text.Lazy.parseOnly (parseDerivation "/nix/store") contents of
    Left e -> error e
    Right drv ->
        Data.Text.IO.writeFile dest
      . Data.Text.Lazy.toStrict
      . Data.Text.Lazy.Builder.toLazyText
      $ buildDerivation drv

test_derivation :: TestTree
test_derivation = testGroup "golden" $ map mk [0..1]
  where
    mk n =
      let
        fp = "tests/samples/example"
        drv = (fp ++ show n ++ ".drv")
        act = (fp ++ show n ++ ".actual")
      in
        goldenVsFile
          ("derivation roundtrip of " ++ drv)
          drv act (processDerivation drv act)

