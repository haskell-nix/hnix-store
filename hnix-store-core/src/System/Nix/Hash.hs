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
module System.Nix.Hash where

import Control.Monad (void)
import Data.Coerce (coerce)
import qualified Data.ByteString as BS
import Data.Hashable (Hashable (..))
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=))
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Marshal.Utils (copyBytes)

data HashAlgorithm = TruncatedSHA256 | MD5

newtype Digest (algo :: HashAlgorithm) = Digest { getDigestBytes :: BS.ByteString }
  deriving (Eq, Ord, Show)

instance Hashable (Digest algo) where
  hashWithSalt s (Digest bytes) = hashWithSalt s bytes
