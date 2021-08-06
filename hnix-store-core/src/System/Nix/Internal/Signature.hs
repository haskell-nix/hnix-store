{-|
Description : Nix-relevant interfaces to NaCl signatures.
-}
{-# language CPP #-}

module System.Nix.Internal.Signature
  ( Signature
  , NarSignature(..)
  )
where


import qualified Data.ByteString                   as Bytes
import           Crypto.Saltine.Core.Sign           ( PublicKey )
import           Crypto.Saltine.Class               ( IsEncoding(..) )

--  2021-05-30: NOTE: Please, clean-up these overloads in ~2022
#if MIN_VERSION_saltine(0,2,0)
import qualified Crypto.Saltine.Internal.Sign as NaClSizes
#else
import qualified Crypto.Saltine.Internal.ByteSizes as NaClSizes
#endif

-- | A NaCl signature.
newtype Signature = Signature ByteString
  deriving (Eq, Ord)

instance IsEncoding Signature where
  decode s
#if MIN_VERSION_saltine(0,2,0)
    | Bytes.length s == NaClSizes.sign_bytes = Just $ Signature s
#else
    | Bytes.length s == NaClSizes.sign = Just $ Signature s
#endif
    | otherwise = Nothing
  encode = coerce

-- | A detached NaCl signature attesting to a nix archive's validity.
data NarSignature = NarSignature
  { -- | The public key used to sign the archive.
    publicKey :: PublicKey
  , -- | The archive's signature.
    sig       :: Signature
  }
  deriving (Eq, Ord)
