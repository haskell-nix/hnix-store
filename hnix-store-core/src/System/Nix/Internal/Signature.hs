{-|
Description : Nix-relevant interfaces to NaCl signatures.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Internal.Signature where


import Data.ByteString (ByteString)
import qualified Data.ByteString as Bytes
import Data.Coerce (coerce)
import Crypto.Saltine.Core.Sign (PublicKey)
import Crypto.Saltine.Class (IsEncoding(..))
import qualified Crypto.Saltine.Internal.ByteSizes as NaClSizes


-- | A NaCl signature.
newtype Signature = Signature ByteString
  deriving (Eq, Ord)

instance IsEncoding Signature where
  decode s
    | Bytes.length s == NaClSizes.sign = Just (Signature s)
    | otherwise = Nothing
  encode = coerce

-- | A detached NaCl signature attesting to a nix archive's validity.
data NarSignature = NarSignature
  { -- | The public key used to sign the archive.
    publicKey :: PublicKey
  , -- | The archive's signature.
    sig :: Signature
  } deriving (Eq, Ord)
