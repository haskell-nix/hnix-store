
module Derivation where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import System.Nix.Derivation (parseDerivation, buildDerivation)

import Data.Default.Class (Default(def))
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Data.Text.Lazy.Builder

processDerivation :: FilePath -> FilePath -> IO ()
processDerivation source dest = do
  contents <- Data.Text.Lazy.IO.readFile source
  either
    fail
    (Data.Text.Lazy.IO.writeFile dest
      . Data.Text.Lazy.Builder.toLazyText
      . buildDerivation def
    )
    (Data.Attoparsec.Text.Lazy.parseOnly
      (parseDerivation def)
      contents
    )

test_derivation :: TestTree
test_derivation =
  testGroup "golden" $ fmap mk [0 .. 1]
 where
  mk :: Int -> TestTree
  mk n =
    goldenVsFile
      ("derivation roundtrip of " <> drv)
      drv
      act
      (processDerivation drv act)
   where
    drv = fp <> show n <> ".drv"
    act = fp <> show n <> ".actual"
    fp  = "tests/samples/example"
