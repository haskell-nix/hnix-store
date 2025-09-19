{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Description : Representation of Nix store paths.
-}

module System.Nix.StorePath
  ( -- * Basic store path types
    StoreDir(..)
  , HasStoreDir(..)
  , StorePath
  , storePathHash
  , storePathName
  , StorePathName
  , unStorePathName
  , StorePathHashPart
  , mkStorePathHashPart
  , unStorePathHashPart
    -- * Manipulating 'StorePathName'
  , InvalidNameError(..)
  , mkStorePathName
  , parseNameText
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
  , parsePathFromText
  , pathParser
    -- * Utilities for tests
  , unsafeMakeStorePath
  , unsafeMakeStorePathHashPart
  ) where

import Control.DeepSeq (NFData)
import Crypto.Hash (HashAlgorithm)
import Data.Attoparsec.Text.Lazy (Parser, (<?>))
import Data.ByteString (ByteString)
import Data.Default.Class (Default(def))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Nix.Base (BaseEncoding(NixBase32))

import Data.Bifunctor qualified
import Data.ByteString.Char8 qualified
import Data.Char qualified
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Attoparsec.Text.Lazy qualified
import System.FilePath qualified

import System.Nix.Base qualified
import System.Nix.Hash qualified
import System.Nix.Base32 qualified

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
  deriving (Eq, Generic, Ord)

instance NFData StorePath

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

instance Show StorePath where
  show s =
    "StorePath"
    <> " "
    <> storePathToFilePath (StoreDir mempty) s

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance NFData StorePathName

-- | The hash algorithm used for store path hashes.
newtype StorePathHashPart = StorePathHashPart
  { -- | Extract the contents of the hash.
    unStorePathHashPart :: ByteString
  }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance NFData StorePathHashPart

-- | Make @StorePathHashPart@ from @ByteString@ (hash part of the @StorePath@)
-- using specific @HashAlgorithm@
mkStorePathHashPart
  :: forall hashAlgo
   . HashAlgorithm hashAlgo
  => ByteString
  -> StorePathHashPart
mkStorePathHashPart =
  StorePathHashPart
  . System.Nix.Hash.mkStorePathHash @hashAlgo

-- | Reason why a path name or output name is not valid
data InvalidNameError
  = EmptyName
  | NameTooLong Int
  | LeadingDot
  | InvalidCharacters Text
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | Reason why a path is not valid
data InvalidPathError
  = PathNameInvalid InvalidNameError
  | HashDecodingFailure String
  | RootDirMismatch
      { rdMismatchExpected :: StoreDir
      , rdMismatchGot      :: StoreDir
      }
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | Make @StorePathName@ from @Text@ (name part of the @StorePath@)
-- or fail with @InvalidNameError@ if it isn't valid
mkStorePathName :: Text -> Either InvalidNameError StorePathName
mkStorePathName = fmap StorePathName . parseNameText

-- | Parse name (either @StorePathName@ or @OutputName@)
parseNameText :: Text -> Either InvalidNameError Text
parseNameText n
  | n == ""
    = Left EmptyName
  | Data.Text.head n == '.'
    = Left LeadingDot
  | nLength > 211
    = Left $ NameTooLong nLength
  | not $ Data.Text.null invalidCharacters
    = Left $ InvalidCharacters invalidCharacters
  | otherwise = pure n
 where
  invalidCharacters = Data.Text.filter (not . validStorePathNameChar) n
  nLength = Data.Text.length n

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

instance Default StoreDir where
  def = StoreDir "/nix/store"

class HasStoreDir r where
  hasStoreDir :: r -> StoreDir

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath :: StoreDir -> StorePath -> RawFilePath
storePathToRawFilePath storeDir StorePath{..} =
  unStoreDir storeDir <> "/" <> hashPart <> "-" <> name
 where
  hashPart = Data.Text.Encoding.encodeUtf8 $ storePathHashPartToText storePathHash
  name     = Data.Text.Encoding.encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StoreDir -> StorePath -> FilePath
storePathToFilePath storeDir = Data.ByteString.Char8.unpack . storePathToRawFilePath storeDir

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StoreDir -> StorePath -> Text
storePathToText storeDir =
  Data.Text.pack
  . Data.ByteString.Char8.unpack
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

-- | Parse `StorePath` from `String`, internal
parsePath'
  :: StoreDir
  -> String
  -> Either InvalidPathError StorePath
parsePath' expectedRoot stringyPath =
  let
    (rootDir, fname) = System.FilePath.splitFileName stringyPath
    (storeBasedHashPart, namePart) = Data.Text.breakOn "-" $ Data.Text.pack fname
    hashPart =
      Data.Bifunctor.bimap
        HashDecodingFailure
        StorePathHashPart
        $ System.Nix.Base.decodeWith NixBase32 storeBasedHashPart
    name =
      Data.Bifunctor.first
        PathNameInvalid
        $ mkStorePathName . Data.Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = init rootDir
    expectedRootS = Data.ByteString.Char8.unpack (unStoreDir expectedRoot)
    storeDir =
      if expectedRootS == rootDir'
        then pure rootDir'
        else Left $ RootDirMismatch
                      { rdMismatchExpected = expectedRoot
                      , rdMismatchGot = StoreDir $ Data.ByteString.Char8.pack rootDir
                      }
  in
    either Left (pure $ StorePath <$> hashPart <*> name) storeDir

-- | Parse `StorePath` from `ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath
  :: StoreDir -- ^ expected @StoreDir@
  -> ByteString
  -> Either InvalidPathError StorePath
parsePath sd = parsePath' sd . Data.ByteString.Char8.unpack

-- | Parse `StorePath` from `Text`, checking
-- that store directory matches `expectedRoot`.
parsePathFromText
  :: StoreDir -- ^ expected @StoreDir@
  -> Text
  -> Either InvalidPathError StorePath
parsePathFromText sd = parsePath' sd . Data.Text.unpack

-- | Attoparsec @StorePath@ @Parser@
pathParser :: StoreDir -> Parser StorePath
pathParser expectedRoot = do
  let expectedRootT =
          Data.Text.pack
        . Data.ByteString.Char8.unpack
        $ unStoreDir expectedRoot

  _ <- Data.Attoparsec.Text.Lazy.string expectedRootT
      <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Data.Attoparsec.Text.Lazy.char '/'
      <?> "Expecting path separator"

  digest <-
    System.Nix.Base.decodeWith NixBase32
    <$> Data.Attoparsec.Text.Lazy.takeWhile1
      (`elem` System.Nix.Base32.digits32)
      <?> "Invalid Base32 part"

  _  <- Data.Attoparsec.Text.Lazy.char '-'
      <?> "Expecting dash (path name separator)"

  c0 <-
    Data.Attoparsec.Text.Lazy.satisfy
      (\c -> c /= '.' && validStorePathNameChar c)
      <?> "Leading path name character is a dot or invalid character"

  rest <-
    Data.Attoparsec.Text.Lazy.takeWhile
    validStorePathNameChar
      <?> "Path name contains invalid character"

  let name =
        Data.Bifunctor.first
          PathNameInvalid
          $ mkStorePathName $ Data.Text.cons c0 rest
      hashPart =
        Data.Bifunctor.bimap
          HashDecodingFailure
          StorePathHashPart
          digest

  either
    (fail . show)
    pure
    (StorePath <$> hashPart <*> name)

-- * Utilities for tests

-- | Paths rarely need to be constructed directly.
-- Prefer @parsePath@ or @parsePathFromText@
unsafeMakeStorePath
  :: StorePathHashPart
  -> StorePathName
  -> StorePath
unsafeMakeStorePath = StorePath

-- | Path hash parts rarely need to be constructed directly.
-- Prefer @mkStorePathHashPart@
-- Used by remote store in wire protocol
unsafeMakeStorePathHashPart
  :: ByteString
  -> StorePathHashPart
unsafeMakeStorePathHashPart = StorePathHashPart
