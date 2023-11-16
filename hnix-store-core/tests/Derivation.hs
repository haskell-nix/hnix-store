
module Derivation where

import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Test.Tasty.Golden              ( goldenVsFile )
import           Test.Tasty.QuickCheck

import           Nix.Derivation                 ( Derivation )
import           System.Nix.StorePath           ( StoreDir(..), StorePath )
import           System.Nix.Derivation          ( parseDerivation
                                                , buildDerivation
                                                )

import Data.Default.Class (Default(def))
import qualified Data.Attoparsec.Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
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
      . buildDerivation def
    )
    (Data.Attoparsec.Text.parseOnly
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

-- TODO(srk): this won't roundtrip as Arbitrary Text
-- contains wild stuff like control characters and UTF8 sequences.
-- Either fix in nix-derivation or use wrapper type
-- (but we use Nix.Derivation.textParser so we need Text for now)
xprop_derivationRoundTrip :: StoreDir -> Derivation StorePath Text -> Property
xprop_derivationRoundTrip = \sd drv ->
  Data.Attoparsec.Text.parseOnly (parseDerivation sd)
   ( Data.Text.Lazy.toStrict
   $ Data.Text.Lazy.Builder.toLazyText
   $ buildDerivation sd drv
   )
  === pure drv

