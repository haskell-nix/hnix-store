{-|
Description : Types and effects for interacting with the Nix store.
Maintainer  : Shea Levy <shea@shealevy.com>
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module System.Nix.Path
  ( FilePathPart(..)
  , Path(..)
  , PathSet
  , StorePathHash(..)
  , SubstitutablePathInfo(..)
  , ValidPathInfo(..)
  , PathName(..)
  , Roots
  , filePathPart
  , pathName
  , toNixBase32
  ) where


import qualified Crypto.Hash.SHA256           as SHA
import           Data.Bits
import qualified Data.ByteArray            as B
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Builder as BSL
import           Data.Hashable             (Hashable (..), hashPtrWithSalt)
import           Data.HashMap.Strict       (HashMap)
import           Data.HashSet              (HashSet)
import           Data.Semigroup               ((<>))
import           Data.Map.Strict           (Map)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Data.Word
import qualified Data.Vector.Unboxed          as UV
import           System.FilePath
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           Text.Regex.Base.RegexLike (makeRegex, matchTest)
import           Text.Regex.TDFA.Text      (Regex)

-- | The name portion of a Nix path.
--
-- Must be composed of a-z, A-Z, 0-9, +, -, ., _, ?, and =, can't
-- start with a ., and must have at least one character.
newtype PathName = PathName
  { pathNameContents :: Text -- ^ The contents of the path name
  } deriving (Eq, Ord, Show, Hashable)

-- | A regular expression for matching a valid 'PathName'
nameRegex :: Regex
nameRegex =
  makeRegex "[a-zA-Z0-9\\+\\-\\_\\?\\=][a-zA-Z0-9\\+\\-\\.\\_\\?\\=]*"

-- | Construct a 'PathName', assuming the provided contents are valid.
pathName :: Text -> Maybe PathName
pathName n = case matchTest nameRegex n of
  True  -> Just $ PathName n
  False -> Nothing


-- | A path in a store.
data Path = Path !StorePathHash !PathName
  deriving (Eq, Ord, Show)

newtype StorePathHash = StorePathHash { getTruncatedHash :: BSL.ByteString }
  deriving (Eq, Ord, Show, Hashable)

instance Hashable Path where
  hashWithSalt s (Path digest name) =
    s `hashWithSalt`
    digest `hashWithSalt` name


type PathSet = HashSet Path

-- | Information about substitutes for a 'Path'.
data SubstitutablePathInfo = SubstitutablePathInfo
  { -- | The .drv which led to this 'Path'.
    deriver      :: !(Maybe Path)
  , -- | The references of the 'Path'
    references   :: !PathSet
  , -- | The (likely compressed) size of the download of this 'Path'.
    downloadSize :: !Integer
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSize      :: !Integer
  } deriving (Eq, Ord, Show)

-- | Information about 'Path'.
data ValidPathInfo = ValidPathInfo
  { -- | Path itself
    path             :: !Path
  , -- | The .drv which led to this 'Path'.
    deriverVP        :: !(Maybe Path)
  , -- | NAR hash
    narHash          :: !Text
  , -- | The references of the 'Path'
    referencesVP     :: !PathSet
  , -- | Registration time should be time_t
    registrationTime :: !Integer
  , -- | The size of the uncompressed NAR serialization of this
    -- 'Path'.
    narSizeVP        :: !Integer
  , -- | Whether the path is ultimately trusted, that is, it's a
    -- derivation output that was built locally.
    ultimate         :: !Bool
  , -- | Signatures
    sigs             :: ![Text]
  , -- | Content-addressed
    -- Store path is computed from a cryptographic hash
    -- of the contents of the path, plus some other bits of data like
    -- the "name" part of the path.
    --
    -- ‘ca’ has one of the following forms:
    -- * ‘text:sha256:<sha256 hash of file contents>’ (paths by makeTextPath() / addTextToStore())
    -- * ‘fixed:<r?>:<ht>:<h>’ (paths by makeFixedOutputPath() / addToStore())
    ca               :: !Text
  } deriving (Eq, Ord, Show)

-- | A valid filename or directory name
newtype FilePathPart = FilePathPart { unFilePathPart :: BSC.ByteString }
  deriving (Eq, Ord, Show)

-- | Construct FilePathPart from Text by checking that there
--   are no '/' or '\\NUL' characters
filePathPart :: BSC.ByteString -> Maybe FilePathPart
filePathPart p = case BSC.any (`elem` ['/', '\NUL']) p of
  False -> Just $ FilePathPart p
  True  -> Nothing

-- | Convert a ByteString to base 32 in the way that Nix does
toNixBase32 :: BSL.ByteString -> BSL.ByteString
toNixBase32 x = BSL.toLazyByteString $ mconcat $ map (BSL.word8 . (symbols UV.!) . fromIntegral) vals
  where vals = byteStringToQuintets x
        symbols = UV.fromList $ map (fromIntegral . fromEnum) $ filter (`notElem` ("eotu" :: String)) $ ['0'..'9'] <> ['a'..'z']
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
type Roots = Map Path Path
