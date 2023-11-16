{-|
Description : Representation of Nix store paths.
-}
{-# language ConstraintKinds #-}
{-# language DeriveAnyClass #-}
{-# language AllowAmbiguousTypes #-}
{-# language DataKinds #-}

module System.Nix.Internal.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , StorePath(..)
  , StorePathName(..)
  , StorePathHashPart(..)
  , mkStorePathHashPart
  , ContentAddressableAddress(..)
  , NarHashMode(..)
  , -- * Manipulating 'StorePathName'
    makeStorePathName
  , validStorePathName
    -- * Reason why a path is not valid
  , InvalidPathError(..)
  , -- * Rendering out 'StorePath's
    storePathToFilePath
  , storePathToRawFilePath
  , storePathToText
  , storePathToNarInfo
  , storePathHashPartToText
  , -- * Parsing 'StorePath's
    parsePath
  , pathParser
  )
where

import Data.Default.Class (Default(def))
import qualified Relude.Unsafe as Unsafe
import           System.Nix.Internal.Hash
import           System.Nix.Internal.Base
import qualified System.Nix.Internal.Base32    as Nix.Base32

import qualified Data.ByteString.Char8         as Bytes.Char8
import qualified Data.Char                     as Char
import qualified Data.Text                     as Text
import           Data.Attoparsec.Text.Lazy      ( Parser
                                                , (<?>)
                                                )
import qualified Data.Attoparsec.Text.Lazy     as Parser.Text.Lazy
import qualified System.FilePath               as FilePath
import           Crypto.Hash                    ( SHA256
                                                , Digest
                                                , HashAlgorithm
                                                )

import Test.QuickCheck

-- | A path in a Nix store.
--
-- From the Nix thesis: A store path is the full path of a store
-- object. It has the following anatomy: storeDir/hashPart-name.
--
-- The store directory is *not* included, and must be known from the
-- context. This matches modern C++ Nix, and also represents the fact
-- that store paths for different store directories cannot be mixed.
data StorePath = StorePath
  { -- | The 160-bit hash digest reflecting the "address" of the name.
    -- Currently, this is a truncated SHA256 hash.
    storePathHash :: !StorePathHashPart
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

instance Arbitrary StorePath where
  arbitrary =
    liftA2 StorePath
      arbitrary
      arbitrary

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance Arbitrary StorePathName where
  arbitrary = StorePathName . toText <$> ((:) <$> s1 <*> listOf sn)
   where
    alphanum = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
    s1       = elements $ alphanum <> "+-_?="
    sn       = elements $ alphanum <> "+-._?="

-- | The hash algorithm used for store path hashes.
newtype StorePathHashPart = StorePathHashPart
  { -- | Extract the contents of the hash.
    unStorePathHashPart :: ByteString
  }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance Arbitrary StorePathHashPart where
  arbitrary = mkStorePathHashPart @SHA256 . Bytes.Char8.pack <$> arbitrary

mkStorePathHashPart
  :: forall hashAlgo
   . HashAlgorithm hashAlgo
  => ByteString
  -> StorePathHashPart
mkStorePathHashPart = StorePathHashPart . mkStorePathHash @hashAlgo

-- | An address for a content-addressable store path, i.e. one whose
-- store path hash is purely a function of its contents (as opposed to
-- paths that are derivation outputs, whose hashes are a function of
-- the contents of the derivation file instead).
--
-- For backwards-compatibility reasons, the same information is
-- encodable in multiple ways, depending on the method used to add the
-- path to the store. These unfortunately result in separate store
-- paths.
data ContentAddressableAddress
  = -- | The path is a plain file added via makeTextPath or
    -- addTextToStore. It is addressed according to a sha256sum of the
    -- file contents.
    Text !(Digest SHA256)
  | -- | The path was added to the store via makeFixedOutputPath or
    -- addToStore. It is addressed according to some hash algorithm
    -- applied to the nar serialization via some 'NarHashMode'.
    Fixed !NarHashMode !SomeNamedDigest
  deriving (Eq, Generic, Ord, Show)

-- | Schemes for hashing a Nix archive.
--
-- For backwards-compatibility reasons, there are two different modes
-- here, even though 'Recursive' should be able to cover both.
data NarHashMode
  = -- | Require the nar to represent a non-executable regular file.
    RegularFile
  | -- | Hash an arbitrary nar, including a non-executable regular
    -- file if so desired.
    Recursive
  deriving (Eq, Enum, Generic, Hashable, Ord, Show)

-- | Reason why a path is not valid
data InvalidPathError =
    EmptyName
  | PathTooLong
  | LeadingDot
  | InvalidCharacter
  | HashDecodingFailure String
  | RootDirMismatch
      { rdMismatchExpected :: StoreDir
      , rdMismatchGot      :: StoreDir
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

makeStorePathName :: Text -> Either InvalidPathError StorePathName
makeStorePathName n =
  if validStorePathName n
    then pure $ StorePathName n
    else Left $ reasonInvalid n

reasonInvalid :: Text -> InvalidPathError
reasonInvalid n
  | n == ""             = EmptyName
  | Text.length n > 211 = PathTooLong
  | Text.head n == '.'  = LeadingDot
  | otherwise           = InvalidCharacter

validStorePathName :: Text -> Bool
validStorePathName n =
  n /= ""
  && Text.length n <= 211
  && Text.head n /= '.'
  && Text.all validStorePathNameChar n

validStorePathNameChar :: Char -> Bool
validStorePathNameChar c =
  any ($ c)
    [ Char.isAsciiLower -- 'a'..'z', isAscii..er probably faster then putting it out
    , Char.isAsciiUpper -- 'A'..'Z'
    , Char.isDigit
    , (`elem` ("+-._?=" :: String))
    ]

-- | Copied from @RawFilePath@ in the @unix@ package, duplicated here
-- to avoid the dependency.
type RawFilePath = ByteString

-- | The path to the store dir
--
-- Many operations need to be parameterized with this, since store paths
-- do not know their own store dir by design.
newtype StoreDir = StoreDir {
    unStoreDir :: RawFilePath
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance Arbitrary StoreDir where
  arbitrary = StoreDir . ("/" <>) . Bytes.Char8.pack <$> arbitrary

instance Default StoreDir where
  def = StoreDir "/nix/store"

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath :: StoreDir -> StorePath -> RawFilePath
storePathToRawFilePath storeDir StorePath{..} =
  unStoreDir storeDir <> "/" <> hashPart <> "-" <> name
 where
  hashPart = encodeUtf8 $ storePathHashPartToText storePathHash
  name     = encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath storeDir = Bytes.Char8.unpack . storePathToRawFilePath storeDir

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText storeDir = toText . Bytes.Char8.unpack . storePathToRawFilePath storeDir

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo :: StorePath -> Bytes.Char8.ByteString
storePathToNarInfo StorePath{..} =
  encodeUtf8 $ encodeWith NixBase32 (coerce storePathHash) <> ".narinfo"

-- | Render a 'StorePathHashPart' as a 'Text'.
-- This is used by remote store / database
-- via queryPathFromHashPart
storePathHashPartToText :: StorePathHashPart -> Text
storePathHashPartToText = encodeWith NixBase32 . unStorePathHashPart

-- | Parse `StorePath` from `Bytes.Char8.ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath
  :: StoreDir
  -> Bytes.Char8.ByteString
  -> Either InvalidPathError StorePath
parsePath expectedRoot x =
  let
    (rootDir, fname) = FilePath.splitFileName . Bytes.Char8.unpack $ x
    (storeBasedHashPart, namePart) = Text.breakOn "-" $ toText fname
    hashPart = bimap
      HashDecodingFailure
      StorePathHashPart
      $ decodeWith NixBase32 storeBasedHashPart
    name = makeStorePathName . Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = Unsafe.init rootDir
    expectedRootS = Bytes.Char8.unpack (unStoreDir expectedRoot)
    storeDir =
      if expectedRootS == rootDir'
        then pure rootDir'
        else Left $ RootDirMismatch
                      { rdMismatchExpected = expectedRoot
                      , rdMismatchGot = StoreDir $ Bytes.Char8.pack rootDir
                      }
  in
    either Left (pure $ StorePath <$> hashPart <*> name) storeDir

pathParser :: StoreDir -> Parser StorePath
pathParser expectedRoot = do
  let expectedRootS = Bytes.Char8.unpack (unStoreDir expectedRoot)

  _ <-
    Parser.Text.Lazy.string (toText expectedRootS)
      <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Parser.Text.Lazy.char '/'
      <?> "Expecting path separator"

  digest <-
    decodeWith NixBase32
    <$> Parser.Text.Lazy.takeWhile1 (`elem` Nix.Base32.digits32)
      <?> "Invalid Base32 part"

  _  <- Parser.Text.Lazy.char '-' <?> "Expecting dash (path name separator)"

  c0 <-
    Parser.Text.Lazy.satisfy (\c -> c /= '.' && validStorePathNameChar c)
      <?> "Leading path name character is a dot or invalid character"

  rest <-
    Parser.Text.Lazy.takeWhile validStorePathNameChar
      <?> "Path name contains invalid character"

  let name = makeStorePathName $ Text.cons c0 rest
      hashPart = bimap
        HashDecodingFailure
        StorePathHashPart
        digest

  either
    (fail . show)
    pure
    (StorePath <$> hashPart <*> name)
