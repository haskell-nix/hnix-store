-- | Shared types

module Nix.Derivation.Types
    ( -- * Types
      Derivation(..)
    , DerivationOutput(..)
    ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Filesystem.Path.CurrentOS (FilePath)
import Prelude hiding (FilePath)

-- | A Nix derivation
data Derivation = Derivation
    { outputs   :: Map Text DerivationOutput
    -- ^ Outputs produced by this derivation where keys are output names
    , inputDrvs :: Map FilePath (Set Text)
    -- ^ Inputs that are derivations where keys specify derivation paths and
    -- values specify which output names are used by this derivation
    , inputSrcs :: Set FilePath
    -- ^ Inputs that are sources
    , platform  :: Text
    -- ^ Platform required for this derivation
    , builder   :: FilePath
    -- ^ Executable to build the derivation
    , args      :: Vector Text
    -- ^ Arguments passed to the executable used to build to derivation
    , env       :: Map Text Text
    -- ^ Environment variables provided to the executable used to build the
    -- derivation
    } deriving (Eq, Ord, Show)

-- | An output of a Nix derivation
data DerivationOutput = DerivationOutput
    { path     :: FilePath
    -- ^ Path where the output will be saved
    , hashAlgo :: Text
    -- ^ Hash used for expected hash computation
    , hash     :: Text
    -- ^ Expected hash
    } deriving (Eq, Ord, Show)
