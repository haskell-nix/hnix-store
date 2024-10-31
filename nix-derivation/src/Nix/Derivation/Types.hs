{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

-- | Shared types

module Nix.Derivation.Types
    ( -- * Types
      Derivation(..)
    , DerivationInputs(..)
    , DerivationOutput(..)
    ) where

import Control.DeepSeq (NFData)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | A Nix derivation
data Derivation fp txt outputName drvOutput drvInputs = Derivation
    { outputs   :: Map outputName drvOutput
    -- ^ Outputs produced by this derivation where keys are output names
    , inputs    :: drvInputs
    -- ^ Inputs (sources and derivations)
    , platform  :: txt
    -- ^ Platform required for this derivation
    , builder   :: txt
    -- ^ Code to build the derivation, which can be a path or a builtin function
    , args      :: Vector txt
    -- ^ Arguments passed to the executable used to build to derivation
    , env       :: Map txt txt
    -- ^ Environment variables provided to the executable used to build the
    -- derivation
    } deriving (Eq, Generic, Ord, Show)

instance ( NFData fp
         , NFData txt
         , NFData outputName
         , NFData drvOutput
         , NFData drvInputs
         )
         => NFData (Derivation fp txt outputName drvOutput drvInputs)

data DerivationInputs fp drvOutput = DerivationInputs
    { drvs :: Map fp (Set drvOutput)
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    , srcs :: Set fp
    -- ^ Inputs that are sources
    } deriving (Eq, Generic, Ord, Show)

instance (NFData a, NFData b) => NFData (DerivationInputs a b)

-- | An output of a Nix derivation
data DerivationOutput fp txt
    = InputAddressedDerivationOutput
        { path     :: fp
        -- ^ Path where the output will be saved
        }
    | FixedDerivationOutput
        { path     :: fp
        -- ^ Path where the output will be saved
        , hashAlgo :: txt
        -- ^ Hash used for expected hash computation
        , hash     :: txt
        -- ^ Expected hash
        }
    | ContentAddressedDerivationOutput
        { hashAlgo :: txt
        -- ^ Hash used for expected hash computation
        }
    deriving (Eq, Generic, Ord, Show)

instance (NFData a, NFData b) => NFData (DerivationOutput a b)
