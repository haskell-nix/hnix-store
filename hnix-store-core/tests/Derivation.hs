
module Derivation where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( goldenVsFile )

import           System.Nix.Derivation          ( parseDerivation
                                                , buildDerivation
                                                )

import qualified Data.Attoparsec.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy.Builder

processDerivation :: FilePath -> FilePath -> IO ()
processDerivation source dest = do
  contents <- Data.Text.IO.readFile source
  either
    fail
    -- It seems to be derivation.
    (Data.Text.IO.writeFile dest
      . toText
      . Data.Text.Lazy.Builder.toLazyText
      . buildDerivation
    )
    (Data.Attoparsec.Text.parseOnly
      (parseDerivation "/nix/store")
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
