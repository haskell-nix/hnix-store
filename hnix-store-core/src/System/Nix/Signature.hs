{-|
Description : Nix-relevant interfaces to NaCl signatures.
-}

module System.Nix.Signature
  ( Signature
  , NarSignature(..)
  ) where

import Crypto.Saltine.Core.Sign (PublicKey)
import Crypto.Saltine.Class (IsEncoding(..))
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import qualified Data.ByteString
import qualified Data.Coerce

import qualified Crypto.Saltine.Internal.Sign as NaClSizes

-- | A NaCl signature.
newtype Signature = Signature ByteString
  deriving (Eq, Generic, Ord, Show)

instance IsEncoding Signature where
  decode s
    | Data.ByteString.length s == NaClSizes.sign_bytes = Just $ Signature s
    | otherwise = Nothing
  encode = Data.Coerce.coerce

-- | A detached NaCl signature attesting to a nix archive's validity.
data NarSignature = NarSignature
  { -- | The public key used to sign the archive.
    publicKey :: PublicKey
  , -- | The archive's signature.
    sig       :: Signature
  }
  deriving (Eq, Generic, Ord, Show)
