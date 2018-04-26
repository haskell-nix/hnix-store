{-|
Description : Trunctions of cryptographic hashes.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
module Crypto.Hash.Truncated where

import Control.Monad (void)
import Data.Coerce (coerce, Coercible)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=))
import Crypto.Hash (Digest)
import Crypto.Hash.IO (HashAlgorithm(..),)
import Data.ByteArray (alloc)
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Marshal.Utils (copyBytes)
#if MIN_VERSION_cryptonite(0,25,0)
import Basement.Block.Mutable (Block)
#else
import Foundation.Array (UArray)
#endif
-- | Hash algorithm 'algo' truncated to 'size' bytes.
newtype Truncated algo (size :: Nat) = Truncated algo

-- | The underlying type of a 'Digest'.
#if MIN_VERSION_cryptonite(0,25,0)
type DigestUnwrapped = Block Word8
#else
type DigestUnwrapped = UArray Word8
#endif

-- | Use the 'HashAlgorithm' instance of 'algo' and truncate the final
-- digest.
--
-- The 'Coercible' constraint adds a little bit of type safety to the
-- pointer munging that goes on under the hood.
instance (HashAlgorithm algo, KnownNat (HashDigestSize algo)
         , KnownNat size, size <= HashDigestSize algo
         , Coercible (Digest algo) DigestUnwrapped
         ) => HashAlgorithm (Truncated algo size) where
  type HashBlockSize (Truncated algo size) = HashBlockSize algo
  type HashDigestSize (Truncated algo size) = size
  type HashInternalContextSize (Truncated algo size) =
    HashInternalContextSize algo
  hashBlockSize = hashBlockSize @algo . coerce
  hashDigestSize _ = fromIntegral $ natVal @size Proxy
  hashInternalContextSize = hashInternalContextSize @algo . coerce
  hashInternalInit = hashInternalInit @algo . coerce
  hashInternalUpdate = hashInternalUpdate @algo . coerce
  hashInternalFinalize cptr dptr = void @_ @DigestUnwrapped $
      alloc (fromIntegral $ natVal @(HashDigestSize algo) Proxy) go
    where
      go :: Ptr (Digest algo) -> IO ()
      go p = do
        hashInternalFinalize (coerce cptr) p
        copyBytes dptr (castPtr p) (fromIntegral $ natVal @size Proxy)
