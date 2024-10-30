{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Shared types

module Nix.Derivation.Types
    ( -- * Types
      Derivation(..)
    , DerivationOutput(..)
    ) where

import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- | A Nix derivation
data Derivation fp txt = Derivation
    { outputs   :: Map txt (DerivationOutput fp txt)
    -- ^ Outputs produced by this derivation where keys are output names
    , inputDrvs :: Map fp (Set txt)
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    , inputSrcs :: Set fp
    -- ^ Inputs that are sources
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

instance (NFData a, NFData b) => NFData (Derivation a b)

-- | An output of a Nix derivation
data DerivationOutput fp txt
    = DerivationOutput
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

instance Functor (DerivationOutput fp) where
  fmap f DerivationOutput{..} = DerivationOutput
    { path = path
    , hashAlgo = f hashAlgo
    , hash = f hash
    }

instance Bifunctor DerivationOutput where
  bimap f g DerivationOutput{..} = DerivationOutput
    { path = f path
    , hashAlgo = g hashAlgo
    , hash = g hash
    }
