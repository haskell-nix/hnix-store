{-|
Description : Derivation types
Maintainer  : srk <srk@48.io>
|-}

module System.Nix.Derivation where


import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import System.Nix.Path

type OutputName = Text

newtype DerivationInputs = DerivationInputs
  { _unDerivationInputs :: HashMap Path (HashSet OutputName)
  } deriving (Eq, Ord, Show)

data Derivation = Derivation
  { _derivationInputs    :: DerivationInputs
  , _derivationOutputs   :: !(HashMap OutputName Path)
    -- | Inputs that are sources
  , _derivationInputSrcs :: !PathSet
  , _derivationPlatform  :: !Text
    -- | Path to builder
  , _derivationBuilder   :: !Path
  , _derivationArgs      :: ![Text]
  , _derivationEnv       :: ![HashMap Text Text]
  } deriving (Eq, Ord, Show)
