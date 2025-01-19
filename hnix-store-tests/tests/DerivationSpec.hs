module DerivationSpec where

import Data.Functor.Identity (Identity(..))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.Hspec.Nix (roundtrips)

import System.Nix.Arbitrary ()
import System.Nix.Arbitrary.Derivation
import System.Nix.Derivation

spec :: Spec
spec = do
  describe "DerivationInput" $ do
    prop "roundtrips to (Set SingleDerivedPath)" $
      -- Order is important, 'Set SingleDerivedPath' is the normal from,
      -- since the arbitrary instance for 'DerivationInput' doesn't
      -- properly avoid empty child maps.
      roundtrips
        (foldMap derivationInputsFromSingleDerivedPath)
        (Identity . derivationInputsToDerivedPaths)

  describe "DerivationOutput" $ do
    prop "roundtrips to FreeformDerivationOutput" $ \storeDir storePathName output -> do
      outputName <- generate $ shortEnoughOutputName storePathName
      roundtrips
        (fromSpecificOutput storeDir storePathName outputName)
        (toSpecificOutput @Maybe storeDir storePathName outputName)
        output

  -- Sometimes infinite loops, not sure why

  -- describe "DerivationOutputs" $ do
  --   prop "roundtrips to FreeformDerivationOutputs" $ verboseCheck $ \storeDir storePathName -> do
  --     outputs <- generate $ shortEnoughOutputs storePathName
  --     _ <- roundtrips
  --       (fromSpecificOutputs storeDir storePathName)
  --       (toSpecificOutputs @Maybe storeDir storePathName)
  --       outputs
  --     pure ()

-- -- | Useful for debugging
-- instance MonadFail (Either String) where
--   fail = Left
