{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Description : Representation of Nix store paths.
-}

module System.Nix.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , StorePath(..)
  , StorePathName(..)
  , StorePathHashPart(..)
  , mkStorePathHashPart
  , ContentAddressableAddress(..)
  , contentAddressableAddressBuilder
  , contentAddressableAddressParser
  , digestBuilder
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

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Default.Class (Default(def))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import GHC.Generics (Generic)

import System.Nix.Base (BaseEncoding(NixBase32))
import System.Nix.Hash (NamedAlgo, SomeNamedDigest(SomeDigest))
import qualified System.Nix.Base
import qualified System.Nix.Hash
import qualified System.Nix.Base32    as Nix.Base32

import qualified Data.Bifunctor
import qualified Data.ByteString.Char8         as Bytes.Char8
import qualified Data.Char
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Builder
import           Data.Attoparsec.Text.Lazy      ( Parser
                                                , (<?>)
                                                )
import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Text.Lazy     as Parser.Text.Lazy
import qualified System.FilePath               as FilePath
import           Crypto.Hash                    ( SHA256
                                                , Digest
                                                , HashAlgorithm
                                                )

import Test.QuickCheck (Arbitrary(arbitrary), listOf, elements)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary(..))
import Test.QuickCheck.Instances ()

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
  arbitrary = StorePathName . Data.Text.pack <$> ((:) <$> s1 <*> listOf sn)
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
  arbitrary =
    mkStorePathHashPart @SHA256
    . Bytes.Char8.pack <$> arbitrary

mkStorePathHashPart
  :: forall hashAlgo
   . HashAlgorithm hashAlgo
  => ByteString
  -> StorePathHashPart
mkStorePathHashPart =
  StorePathHashPart
  . System.Nix.Hash.mkStorePathHash @hashAlgo

-- TODO(srk): split into its own module + .Builder/.Parser
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

deriving via GenericArbitrary ContentAddressableAddress
  instance Arbitrary ContentAddressableAddress

-- | Builder for `ContentAddressableAddress`
contentAddressableAddressBuilder :: ContentAddressableAddress -> Builder
contentAddressableAddressBuilder (Text digest) =
  "text:"
  <> digestBuilder digest
contentAddressableAddressBuilder (Fixed narHashMode (SomeDigest (digest :: Digest hashAlgo))) =
  "fixed:"
  <> (if narHashMode == Recursive then "r:" else mempty)
--  <> Data.Text.Lazy.Builder.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> digestBuilder digest

-- | Builder for @Digest@s
digestBuilder :: forall hashAlgo . (NamedAlgo hashAlgo) => Digest hashAlgo -> Builder
digestBuilder digest =
  Data.Text.Lazy.Builder.fromText (System.Nix.Hash.algoName @hashAlgo)
  <> ":"
  <> Data.Text.Lazy.Builder.fromText
      (System.Nix.Hash.encodeDigestWith NixBase32 digest)

-- | Parser for content addressable field
contentAddressableAddressParser :: Data.Attoparsec.ByteString.Char8.Parser ContentAddressableAddress
contentAddressableAddressParser = caText <|> caFixed
  where
  -- | Parser for @text:sha256:<h>@
  --caText :: Parser ContentAddressableAddress
  caText = do
    _      <- "text:sha256:"
    digest <- System.Nix.Hash.decodeDigestWith @SHA256 NixBase32 <$> parseHash
    either fail pure $ Text <$> digest

  -- | Parser for @fixed:<r?>:<ht>:<h>@
  --caFixed :: Parser ContentAddressableAddress
  caFixed = do
    _           <- "fixed:"
    narHashMode <- (Recursive <$ "r:") <|> (RegularFile <$ "")
    digest      <- parseTypedDigest
    either fail pure $ Fixed narHashMode <$> digest

  --parseTypedDigest :: Parser (Either String SomeNamedDigest)
  parseTypedDigest = System.Nix.Hash.mkNamedDigest <$> parseHashType <*> parseHash

  --parseHashType :: Parser Text
  parseHashType =
    Data.Text.Encoding.decodeUtf8
    <$> ("sha256" <|> "sha512" <|> "sha1" <|> "md5") <* (":" <|> "-")

  --parseHash :: Parser Text
  parseHash =
    Data.Text.Encoding.decodeUtf8
    <$> Data.Attoparsec.ByteString.Char8.takeWhile1 (/= ':')

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

deriving via GenericArbitrary NarHashMode
  instance Arbitrary NarHashMode

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
  | n == ""                  = EmptyName
  | Data.Text.length n > 211 = PathTooLong
  | Data.Text.head n == '.'  = LeadingDot
  | otherwise                = InvalidCharacter

validStorePathName :: Text -> Bool
validStorePathName n =
  n /= ""
  && Data.Text.length n <= 211
  && Data.Text.head n /= '.'
  && Data.Text.all validStorePathNameChar n

validStorePathNameChar :: Char -> Bool
validStorePathNameChar c =
  any ($ c)
    [ Data.Char.isAsciiLower -- 'a'..'z', isAscii..er probably faster then putting it out
    , Data.Char.isAsciiUpper -- 'A'..'Z'
    , Data.Char.isDigit
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
  hashPart = Data.Text.Encoding.encodeUtf8 $ storePathHashPartToText storePathHash
  name     = Data.Text.Encoding.encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath storeDir = Bytes.Char8.unpack . storePathToRawFilePath storeDir

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText storeDir =
  Data.Text.pack
  . Bytes.Char8.unpack
  . storePathToRawFilePath storeDir

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo :: StorePath -> ByteString
storePathToNarInfo StorePath{..} =
  Data.Text.Encoding.encodeUtf8
  $ System.Nix.Base.encodeWith NixBase32
      (unStorePathHashPart storePathHash) <> ".narinfo"

-- | Render a 'StorePathHashPart' as a 'Text'.
-- This is used by remote store / database
-- via queryPathFromHashPart
storePathHashPartToText :: StorePathHashPart -> Text
storePathHashPartToText =
  System.Nix.Base.encodeWith NixBase32 . unStorePathHashPart

-- | Parse `StorePath` from `Bytes.Char8.ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath
  :: StoreDir
  -> Bytes.Char8.ByteString
  -> Either InvalidPathError StorePath
parsePath expectedRoot x =
  let
    (rootDir, fname) = FilePath.splitFileName . Bytes.Char8.unpack $ x
    (storeBasedHashPart, namePart) = Data.Text.breakOn "-" $ Data.Text.pack fname
    hashPart = Data.Bifunctor.bimap
      HashDecodingFailure
      StorePathHashPart
      $ System.Nix.Base.decodeWith NixBase32 storeBasedHashPart
    name = makeStorePathName . Data.Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = init rootDir
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
    Parser.Text.Lazy.string (Data.Text.pack expectedRootS)
      <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Parser.Text.Lazy.char '/'
      <?> "Expecting path separator"

  digest <-
    System.Nix.Base.decodeWith NixBase32
    <$> Parser.Text.Lazy.takeWhile1 (`elem` Nix.Base32.digits32)
      <?> "Invalid Base32 part"

  _  <- Parser.Text.Lazy.char '-' <?> "Expecting dash (path name separator)"

  c0 <-
    Parser.Text.Lazy.satisfy (\c -> c /= '.' && validStorePathNameChar c)
      <?> "Leading path name character is a dot or invalid character"

  rest <-
    Parser.Text.Lazy.takeWhile validStorePathNameChar
      <?> "Path name contains invalid character"

  let name = makeStorePathName $ Data.Text.cons c0 rest
      hashPart = Data.Bifunctor.bimap
        HashDecodingFailure
        StorePathHashPart
        digest

  either
    (fail . show)
    pure
    (StorePath <$> hashPart <*> name)
