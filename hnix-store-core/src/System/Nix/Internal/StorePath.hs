{-|
Description : Representation of Nix store paths.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-} -- Needed for GHC 8.4.4 for some reason


module System.Nix.Internal.StorePath where


import System.Nix.Hash
  ( HashAlgorithm (SHA256)
  , Digest
  , BaseEncoding(..)
  , encodeInBase
  , decodeBase
  )
import System.Nix.Internal.Old as Old
import System.Nix.Internal.TruncatedHash as TruncatedHash
import System.Nix.Internal.Base32 (dictNixBase32)

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Char as Char
import Data.Hashable (Hashable(..))
import Data.HashSet (HashSet)

-- import Data.Attoparsec.Text.Lazy (Parser, (<?>))
import Data.Attoparsec.ByteString.Lazy (Parser, (<?>))

import qualified Data.Attoparsec.Text.Lazy as Parse.Text
import qualified Data.Attoparsec.ByteString.Lazy as Parse.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as Parse.ByteString (char)
import qualified System.FilePath as FilePath

-- | A path in a Nix store.
--
-- From the Nix thesis: A store path is the full path of a store
-- object. It has the following anatomy: storeDir/hashPart-name.
--
-- @storeDir@: The root of the Nix store (e.g. \/nix\/store).
--
-- See the 'StoreDir' haddocks for details on why we represent this at
-- the type level.
data StorePath = StorePath
  { -- | The 160-bit hash digest reflecting the "address" of the name.
    -- Currently, this is a truncated SHA256 hash.
    storePathHash :: !(Digest StorePathHashAlgo)
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  , -- | Root of the store
    storePathRoot :: !FilePath
  } deriving (Eq, Ord)

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

instance Show StorePath where
  show p = ByteString.Char8.unpack $ storePathToRawFilePath p

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character.
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Hashable, Ord)

-- | The hash algorithm used for store path hashes.
type StorePathHashAlgo = 'Old.Truncated 20 'SHA256

-- | A set of 'StorePath's.
type StorePathSet = HashSet StorePath

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
    Text !(Digest 'SHA256)
  -- | -- | The path was added to the store via makeFixedOutputPath or
  --   -- addToStore. It is addressed according to some hash algorithm
  --   -- applied to the nar serialization via some 'NarHashMode'.
  --   Fixed !NarHashMode !(Digest 'HashAlgorithm)

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

makeStorePathName :: Text -> Either String StorePathName
makeStorePathName n = case validStorePathName n of
  True  -> Right $ StorePathName n
  False -> Left $ reasonInvalid n

reasonInvalid :: Text -> String
reasonInvalid n | n == ""            = "Empty name"
reasonInvalid n | (Text.length n > 211) = "Path too long"
reasonInvalid n | (Text.head n == '.')  = "Leading dot"
reasonInvalid _ | otherwise          = "Invalid character"

validStorePathName :: Text -> Bool
validStorePathName "" = False
validStorePathName n  = (Text.length n <= 211)
                        && Text.head n /= '.'
                        && Text.all validateStorePathNameChar n

validateStorePathNameChar :: Char -> Bool
validateStorePathNameChar c =
  bool
  False
  (any ($ c)
   [ Char.isLower
   , Char.isDigit
   , Char.isUpper
   , (`elem` ("+-._?=" :: String))])
  (Char.isAscii c)

-- | Copied from @RawFilePath@ in the @unix@ package, duplicated here
-- to avoid the dependency.
type RawFilePath = ByteString

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath
  :: StorePath
  -> RawFilePath
storePathToRawFilePath StorePath{..} = root <> "/" <> hashPart <> "-" <> name
  where
    root = ByteString.Char8.pack storePathRoot
    hashPart = encodeUtf8 $ encodeInBase Base32 storePathHash
    name = encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath
  :: StorePath
  -> FilePath
storePathToFilePath = ByteString.Char8.unpack . storePathToRawFilePath

-- | Render a 'StorePath' as a 'Text'.
storePathToText
  :: StorePath
  -> Text
storePathToText = Text.pack . ByteString.Char8.unpack . storePathToRawFilePath

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo
  :: StorePath
  -> ByteString.Char8.ByteString
storePathToNarInfo StorePath{..} = storePathHashInNixBase <> ".narinfo"
 where
  storePathHashInNixBase = encodeUtf8 $ encodeInBase Base32 storePathHash

-- | Parse `StorePath` from `ByteString.Char8.ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath
  :: FilePath
  -> ByteString.Char8.ByteString
  -> Either String StorePath
parsePath expectedRoot x =
  let
    (rootDir, fname) = FilePath.splitFileName . ByteString.Char8.unpack $ x
    (digestPart, namePart) = Text.breakOn "-" $ Text.pack fname
    digest = decodeBase Base32 digestPart
    name = makeStorePathName . Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = init rootDir
    storeDir = if expectedRoot == rootDir'
      then Right rootDir'
      else Left $ "Root store dir mismatch, expected '" <> expectedRoot <> "', got '" <> rootDir' <> "'."
  in
    StorePath <$> digest <*> name <*> storeDir

--  2021-01-30: NOTE: Converted Text parser to ByteString. This currently uses Char8
pathParser :: FilePath -> Parser StorePath
pathParser expectedRoot = do
  _ <- Parse.ByteString.string (Text.pack expectedRoot)
    <?> "Store root mismatch" -- e.g. /nix/store

  _ <- Parse.ByteString.char '/'
    <?> "Expecting path separator"

  digest <- decodeBase Base32
    <$> Parse.ByteString.takeWhile1 (`elem` dictNixBase32)
    <?> "Invalid Base32 part"

  _ <- Parse.ByteString.char '-'
    <?> "Expecting dash (path name separator)"

  c0 <- Parse.ByteString.satisfy (\c -> c /= '.' && validateStorePathNameChar c)
    <?> "Leading path name character is a dot or invalid character"

  rest <- Parse.ByteString.takeWhile validateStorePathNameChar
    <?> "Path name contains invalid character"

  let name = makeStorePathName $ Text.cons c0 rest

  either fail return
    $ StorePath <$> digest <*> name <*> pure expectedRoot
