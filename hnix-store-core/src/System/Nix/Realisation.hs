{-|
Description : Derivation realisations
-}

module System.Nix.Realisation (
    BuildTraceKey(..)
  , BuildTraceKeyError(..)
  , buildTraceKeyBuilder
  , buildTraceKeyParser
  , Realisation(..)
  , RealisationWithId(..)
  ) where

import Crypto.Hash (Digest)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Dependent.Sum (DSum)
import GHC.Generics (Generic)
import System.Nix.Hash (HashAlgo)
import System.Nix.OutputName (OutputName, InvalidNameError)
import System.Nix.Signature (Signature)
import System.Nix.StorePath (StorePath)

import Data.Bifunctor qualified
import Data.Text qualified
import Data.Text.Lazy.Builder qualified
import System.Nix.Hash qualified

-- | Output of the derivation
data BuildTraceKey a = BuildTraceKey
  { buildTraceKeyHash :: DSum HashAlgo Digest
  -- ^ Hash modulo of the derivation
  , buildTraceKeyOutput :: a
  -- ^ Output (either a OutputName or StorePatH)
  } deriving (Eq, Generic, Ord, Show)

data BuildTraceKeyError
  = BuildTraceKeyError_Digest String
  | BuildTraceKeyError_Name InvalidNameError
  | BuildTraceKeyError_NoExclamationMark
  | BuildTraceKeyError_NoColon
  | BuildTraceKeyError_TooManyParts [Text]
  deriving (Eq, Ord, Show)

buildTraceKeyParser
  :: (Text -> Either InvalidNameError outputName)
  -> Text
  -> Either BuildTraceKeyError (BuildTraceKey outputName)
buildTraceKeyParser outputName dOut =
  case Data.Text.splitOn (Data.Text.singleton '!') dOut of
    [] -> Left BuildTraceKeyError_NoColon
    [sriHash, oName] -> do
      hash <-
        case Data.Text.splitOn (Data.Text.singleton ':') sriHash of
          [] -> Left BuildTraceKeyError_NoColon
          [hashName, digest] ->
            Data.Bifunctor.first
              BuildTraceKeyError_Digest
              $ System.Nix.Hash.mkNamedDigest hashName digest
          x -> Left $ BuildTraceKeyError_TooManyParts x
      name <-
        Data.Bifunctor.first
          BuildTraceKeyError_Name
          $ outputName oName

      pure $ BuildTraceKey hash name
    x -> Left $ BuildTraceKeyError_TooManyParts x

buildTraceKeyBuilder
  :: (outputName -> Text)
  -> BuildTraceKey outputName
  -> Builder
buildTraceKeyBuilder outputName BuildTraceKey{..} =
     System.Nix.Hash.algoDigestBuilder buildTraceKeyHash
  <> Data.Text.Lazy.Builder.singleton '!'
  <> Data.Text.Lazy.Builder.fromText (outputName buildTraceKeyOutput)

-- | Build realisation context
--
-- realisationId is ommited since it is a key
-- of type @BuildTraceKey OutputName@ so
-- we will use @RealisationWithId@ newtype
data Realisation = Realisation
  { realisationOutPath :: StorePath
  -- ^ Output path
  , realisationSignatures :: Set Signature
  -- ^ Signatures
  , realisationDependencies :: Map (BuildTraceKey OutputName) StorePath
  -- ^ Dependent realisations required for this one to be valid
  } deriving (Eq, Generic, Ord, Show)

-- | For wire protocol
--
-- We store this normalized in @Build.buildResultBuiltOutputs@
-- as @Map (BuildTraceKey OutputName) Realisation@
-- but wire protocol needs it de-normalized so we
-- need a special (From|To)JSON instances for it
newtype RealisationWithId = RealisationWithId
  { unRealisationWithId :: (BuildTraceKey OutputName, Realisation)
  }
  deriving (Eq, Generic, Ord, Show)
