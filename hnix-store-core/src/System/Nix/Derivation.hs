{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Shared types

module System.Nix.Derivation
    ( -- * Types
      Derivation'(..)
    , Derivation
    , BasicDerivation

    , DerivationOutput(..)

    , DerivationInputs(..)
    , derivationInputsFromSingleDerivedPath
    , derivationInputsToDerivedPaths

    , DerivedPathMap(..)
    , ChildNode(..)
    , derivedPathMapFromSingleDerivedPathBuilt
    , derivedPathMapToSet
    ) where

import Control.DeepSeq (NFData(..))
import Crypto.Hash (Digest)
import Data.Dependent.Sum (DSum(..))
import Data.Map (Map)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal
import Data.Set (Set)
import qualified Data.Set
import Data.Some (Some(..))
import Data.GADT.DeepSeq (GNFData(..))
import Data.Text (Text)
import Data.These (These(..), fromThese)
import Data.Vector (Vector)
import GHC.Generics (Generic)

import System.Nix.ContentAddress (ContentAddressMethod)
import System.Nix.Hash (HashAlgo)
import System.Nix.DerivedPath (SingleDerivedPath(..))
import System.Nix.StorePath (StorePath, StorePathName)
import System.Nix.OutputName (OutputName)

data Derivation' inputs output = Derivation
    { name      :: StorePathName
    -- ^ Name of the derivation, needed for calculating output paths
    , outputs   :: Map OutputName output
    -- ^ Outputs produced by this derivation where keys are output names
    , inputs    :: inputs
    -- ^ Inputs (sources and derivations)
    , platform  :: Text
    -- ^ Platform required for this derivation
    , builder   :: Text
    -- ^ Code to build the derivation, which can be a path or a builtin function
    , args      :: Vector Text
    -- ^ Arguments passed to the executable used to build to derivation
    , env       :: Map Text Text
    -- ^ Environment variables provided to the executable used to build the
    -- derivation
    } deriving (Eq, Generic, Ord, Show)

instance (NFData inputs, NFData output) => NFData (Derivation' inputs output)

-- | A regular Nix derivation
type Derivation = Derivation' DerivationInputs DerivationOutput

-- | A Nix derivation that only depends on other store objects directly,
-- not (the outputs of) other derivations
type BasicDerivation = Derivation' (Set StorePath) DerivationOutput

----------------

data DerivationInputs = DerivationInputs
    { srcs :: Set StorePath
    -- ^ Inputs that are sources
    , drvs :: DerivedPathMap
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    } deriving (Eq, Generic, Ord, Show)

instance NFData DerivationInputs

instance Semigroup DerivationInputs where
  DerivationInputs x0 x1 <> DerivationInputs y0 y1 = DerivationInputs
    (x0 <> y0)
    (x1 <> y1)

instance Monoid DerivationInputs where
  mempty = DerivationInputs mempty mempty

derivationInputsFromSingleDerivedPath :: SingleDerivedPath -> DerivationInputs
derivationInputsFromSingleDerivedPath = \case
  SingleDerivedPath_Opaque storePath -> DerivationInputs
    { srcs = Data.Set.singleton storePath
    , drvs = mempty
    }
  SingleDerivedPath_Built drvDPath outputName -> DerivationInputs
    { srcs = mempty
    , drvs = derivedPathMapFromSingleDerivedPathBuilt drvDPath outputName
    }

derivationInputsToDerivedPaths :: DerivationInputs -> Set SingleDerivedPath
derivationInputsToDerivedPaths inputs =
   Data.Set.mapMonotonic SingleDerivedPath_Opaque (srcs inputs)
   <>
   derivedPathMapToSet (drvs inputs)

-- | A recursive map to handle dependencies on dynamic derivations in
-- addition to static ones
newtype DerivedPathMap = DerivedPathMap
  { unDerivedPathMap :: MonoidalMap StorePath ChildNode
  } deriving (Eq, Generic, Ord, Show)
    deriving newtype (Semigroup, Monoid)

instance NFData DerivedPathMap

newtype ChildNode = ChildNode
  { unChildNode :: These (Set OutputName) (MonoidalMap OutputName ChildNode)
  } deriving (Eq, Generic, Ord, Show)
    deriving newtype (Semigroup)

instance NFData ChildNode

derivedPathMapFromSingleDerivedPathBuilt :: SingleDerivedPath -> OutputName -> DerivedPathMap
derivedPathMapFromSingleDerivedPathBuilt drvDPath outputName0 = go drvDPath $ ChildNode $ This $ Data.Set.singleton outputName0
 where
  go :: SingleDerivedPath -> ChildNode -> DerivedPathMap
  go d child = case d of
    SingleDerivedPath_Opaque drvPath -> DerivedPathMap $ Data.Map.Monoidal.singleton drvPath child
    SingleDerivedPath_Built nestedPath nestedOutputName -> go nestedPath $ ChildNode $ That $ Data.Map.Monoidal.singleton nestedOutputName child

derivedPathMapToSet :: DerivedPathMap -> Set SingleDerivedPath
derivedPathMapToSet (DerivedPathMap m) = Data.Set.unions $ fmap
    (\(p, c) -> go (SingleDerivedPath_Opaque p) c)
    (Data.Map.Monoidal.toList m)
 where
   go :: SingleDerivedPath -> ChildNode -> Set SingleDerivedPath
   go accum (ChildNode child) =
        Data.Set.mapMonotonic (SingleDerivedPath_Built accum) shallows
        <>
        Data.Set.unions (fmap
          (\(outputName, child') -> go (SingleDerivedPath_Built accum outputName) child')
          $ Data.Map.Monoidal.toList deeps)
     where (shallows, deeps) = fromThese mempty mempty child

----------------

-- | An output of a Nix derivation
data DerivationOutput
    = InputAddressedDerivationOutput
        { path     :: StorePath
        -- ^ Path where the output will be saved
        }
    | FixedDerivationOutput
        { method   :: ContentAddressMethod
        -- ^ How this output is serialized into a hash / what sort of CA
        -- store path is used.
        , hash     :: DSum HashAlgo Digest
        -- ^ Expected hash of this output
        }
    | ContentAddressedDerivationOutput
        { method   :: ContentAddressMethod
        -- ^ How this output is serialized into a hash / what sort of CA
        -- store path is used.
        , hashAlgo :: Some HashAlgo
        -- ^ What sort of hash function is used with the above
        -- content-addressing method to produce the (content-addressed)
        -- store path we'll use for the output.
        }
    deriving (Eq, Generic, Ord, Show)

instance NFData DerivationOutput

-- | TODO this should go in `dependent-sum`
instance (GNFData k, GNFData v) => NFData (DSum k v) where
  rnf (x :=> y) = grnf x `seq` grnf y
-- | TODO this needs a home
instance GNFData Digest where
  grnf = rnf
