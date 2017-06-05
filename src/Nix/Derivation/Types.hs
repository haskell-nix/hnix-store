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

data Derivation = Derivation
    { outputs   :: Map Text DerivationOutput
    , inputDrvs :: Map FilePath (Set Text)
    , inputSrcs :: Set FilePath
    , platform  :: Text
    , builder   :: FilePath
    , args      :: Vector Text
    , env       :: Map Text Text
    } deriving (Eq, Ord, Show)

data DerivationOutput = DerivationOutput
    { path     :: FilePath
    , hashAlgo :: Text
    , hash     :: Text
    } deriving (Eq, Ord, Show)
