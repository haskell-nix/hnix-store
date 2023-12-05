{-|
Description : Derivation realisations
-}

module System.Nix.Realisation (
    DerivationOutput(..)
  , Realisation(..)
  ) where

import Crypto.Hash (Digest)
import Data.Map (Map)
import Data.Set (Set)
import Data.Dependent.Sum (DSum)
import GHC.Generics (Generic)
import System.Nix.Hash (HashAlgo)
import System.Nix.OutputName (OutputName)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath)

-- | Output of the derivation
data DerivationOutput outputName = DerivationOutput
  { derivationOutputHash :: DSum HashAlgo Digest
  -- ^ Hash modulo of the derivation
  , derivationOutputName :: outputName
  -- ^ Name of the output
  } deriving (Eq, Generic, Ord, Show)

-- | Build realisation context
--
-- realisationId is ommited since it is a key
-- of type @DerivationOutput OutputName@ so
-- we will use a tuple like @(DerivationOutput OutputName, Realisation)@
-- instead.
data Realisation = Realisation
  { realisationOutPath :: StorePath
  -- ^ Output path
  , realisationSignatures :: Set Signature
  -- ^ Signatures
  , realisationDependencies :: Map (DerivationOutput OutputName) StorePath
  -- ^ Dependent realisations required for this one to be valid
  } deriving (Eq, Generic, Ord, Show)
