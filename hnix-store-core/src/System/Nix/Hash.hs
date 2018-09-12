{-|
Description : Nix-style hashes (truncated sha256)
Maintainer  : Shea Levy <shea@shealevy.com>
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Nix.Hash (

  -- * Introduce hashes for the store
    hash
  , hashlazy
  , fromBase32

  -- * cryptohash-sha256 style incremental hash building
  , init
  , update
  , finalize

  -- * Internal
  , StorePathHash (..)
  , truncate52
  , toNixBase32

  ) where

import qualified Crypto.Hash.SHA256           as SHA
import           Data.Bits
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Builder as BSL
import           Data.Char
import qualified Data.Hashable                as Hashable
import           Data.Semigroup               ((<>))
import           Data.Word
import qualified Data.Vector.Unboxed          as UV
import           Prelude                      hiding (init)

-- | A string, file, or NAR hash in the format
--   used in prefixing files in the nix store
newtype StorePathHash =
  StorePathHash { getTruncatedHash :: BS.ByteString }
  deriving (Eq, Hashable.Hashable, Ord, Show)


init :: SHA.Ctx
init = SHA.init


update :: SHA.Ctx -> BS.ByteString -> SHA.Ctx
update = SHA.update


finalize :: SHA.Ctx -> StorePathHash
finalize ctx = StorePathHash . truncate52 $ SHA.finalize ctx


hash :: BS.ByteString -> StorePathHash
hash bs = StorePathHash . BSL.toStrict . toNixBase32 . BSL.fromStrict . truncate' $ SHA.hash bs


hashlazy :: BSL.ByteString -> StorePathHash
hashlazy bs = StorePathHash . BSL.toStrict . toNixBase32 . BSL.fromStrict . truncate' $ SHA.hashlazy bs


-- | Import and validate a store path hash
fromBase32 :: BS.ByteString -> Maybe StorePathHash
fromBase32 = validateRawDigest . StorePathHash
  where validateRawDigest = Just
  -- TODO: What should we check for? Only valid base32 chars?



truncate52
  :: BS.ByteString
     -- ^ A sha256 hash
  -> BS.ByteString
truncate52 digest =
  -- Truncate 52 bits by dropping 6 bytes worth of Word8's,
  -- then masking 4 bits off of the 7th Word8
  case BS.uncons (BS.drop (52 `div` 8) digest) of
    Nothing     -> BS.empty -- We received an hash with unexpectedly short length
    Just (x,xs) -> BS.cons (mask4bits .&. x) xs
    where mask4bits = 2^5 - 1 :: Word8

truncate' :: BS.ByteString -> BS.ByteString
truncate' = BS.take 20


-- | Convert a ByteString to base 32 in the way that Nix does
toNixBase32 :: BSL.ByteString -> BSL.ByteString
toNixBase32 x = BSL.toLazyByteString $ mconcat $ map (BSL.word8 . (symbols UV.!) . fromIntegral) vals
  where vals = byteStringToQuintets x
        symbols = UV.fromList $ map (fromIntegral . ord) $ filter (`notElem` ("eotu" :: String)) $ ['0'..'9'] <> ['a'..'z']
        -- See https://github.com/NixOS/nix/blob/6f1743b1a5116ca57a60b481ee4083c891b7a334/src/libutil/hash.cc#L109
        byteStringToQuintets :: BSL.ByteString -> [Word8]
        byteStringToQuintets hash = map f [len-1, len-2 .. 0]
          where hashSize = fromIntegral $ BSL.length hash
                len = (hashSize * 8 - 1) `div` 5 + 1
                f n = let b = n * 5
                          (i, j) = b `divMod` 8
                          j' = fromIntegral j
                          --TODO: This is probably pretty slow; replace with something that doesn't use BSL.index
                          c = ((hash `BSL.index` i) `shift` (-j')) .|. (if i >= hashSize - 1 then 0 else (hash `BSL.index` (i + 1)) `shift` (8 - j'))
                      in c .&. 0x1f
