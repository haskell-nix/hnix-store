{-|
Description : Derivation types
Maintainer  : srk <srk@48.io>
|-}

module System.Nix.Derivation (
      BasicDerivation(..)
    ) where


import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import System.Nix.Path

data BasicDerivation = BasicDerivation
  { -- | Derivation outputs
    outputs   ::  !(HashMap Text Path)
  , -- | Inputs that are sources
    inputSrcs :: !PathSet
  , -- | Platform
    platform  :: !Text
  , -- | Path to builder
    builder   :: !Path
  , -- | Arguments
    args      :: ![Text]
  , -- | Environment
    env       :: ![HashMap Text Text]
  } deriving (Eq, Ord, Show)
