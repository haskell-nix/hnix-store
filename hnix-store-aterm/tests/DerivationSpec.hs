module DerivationSpec where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (xprop)
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.Derivation (parseDerivation, buildDerivation)

import Data.Attoparsec.Text qualified
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified

-- TODO(srk): this won't roundtrip as Arbitrary Text
-- contains wild stuff like control characters and UTF8 sequences.
-- Either fix in hnix-store-aterm or use wrapper type
-- (but we use Nix.Derivation.textParser so we need Text for now)
spec :: Spec
spec = do
  describe "Derivation" $ do
    xprop "roundtrips via Text" $ \sd ->
      roundtrips
        ( Data.Text.Lazy.toStrict
        . Data.Text.Lazy.Builder.toLazyText
        . buildDerivation sd
        )
        (Data.Attoparsec.Text.parseOnly (parseDerivation sd))
