{-|
Description : Representation of Nix store paths.
-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module System.Nix.Internal.StorePath where
import           System.Nix.Internal.Hash       ( HashAlgorithm(SHA256)
                                                , Digest
                                                , SomeNamedDigest
                                                , mkStorePathHash
                                                )


import qualified System.Nix.Internal.Base32    as Nix.Base32

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Char8         as Bytes.Char8
import qualified Data.Char                     as Char
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
                                                ( encodeUtf8 )
import           Data.Attoparsec.Text.Lazy      ( Parser
                                                , (<?>)
                                                )
import qualified Data.Attoparsec.Text.Lazy     as Parser.Text.Lazy
import qualified System.FilePath               as FilePath
import           Data.Hashable                  ( Hashable(..) )
import           Data.HashSet                   ( HashSet )
import           System.Nix.Internal.Base
import           Data.Coerce                    ( coerce )

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
    storePathHash :: !StorePathHashPart
  , -- | The (typically human readable) name of the path. For packages
    -- this is typically the package name and version (e.g.
    -- hello-1.2.3).
    storePathName :: !StorePathName
  , -- | Root of the store
    storePathRoot :: !FilePath
  }
  deriving (Eq, Ord)

instance Hashable StorePath where
  hashWithSalt s StorePath{..} =
    s `hashWithSalt` storePathHash `hashWithSalt` storePathName

instance Show StorePath where
  show p = Bytes.Char8.unpack $ storePathToRawFilePath p

-- | The name portion of a Nix path.
--
-- 'unStorePathName' must only contain a-zA-Z0-9+._?=-, can't start
-- with a -, and must have at least one character (i.e. it must match
-- 'storePathNameRegex').
newtype StorePathName = StorePathName
  { -- | Extract the contents of the name.
    unStorePathName :: Text
  } deriving (Eq, Hashable, Ord)

-- | The hash algorithm used for store path hashes.
newtype StorePathHashPart = StorePathHashPart ByteString
  deriving (Eq, Hashable, Ord, Show)

mkStorePathHashPart :: ByteString -> StorePathHashPart
mkStorePathHashPart = coerce . mkStorePathHash @'SHA256

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
  | -- | The path was added to the store via makeFixedOutputPath or
    -- addToStore. It is addressed according to some hash algorithm
    -- applied to the nar serialization via some 'NarHashMode'.
    Fixed !NarHashMode !SomeNamedDigest

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
makeStorePathName n =
  if validStorePathName n
    then Right $ StorePathName n
    else Left $ reasonInvalid n

reasonInvalid :: Text -> String
reasonInvalid n
  | n == ""          = "Empty name"
  | Text.length n > 211 = "Path too long"
  | Text.head n == '.'  = "Leading dot"
  | otherwise        = "Invalid character"

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

-- | Render a 'StorePath' as a 'RawFilePath'.
storePathToRawFilePath :: StorePath -> RawFilePath
storePathToRawFilePath StorePath{..} =
  root <> "/" <> hashPart <> "-" <> name
 where
  root     = Bytes.Char8.pack storePathRoot
  hashPart = Text.encodeUtf8 $ encodeWith NixBase32 $ coerce storePathHash
  name     = Text.encodeUtf8 $ unStorePathName storePathName

-- | Render a 'StorePath' as a 'FilePath'.
storePathToFilePath :: StorePath -> FilePath
storePathToFilePath = Bytes.Char8.unpack . storePathToRawFilePath

-- | Render a 'StorePath' as a 'Text'.
storePathToText :: StorePath -> Text
storePathToText = Text.pack . Bytes.Char8.unpack . storePathToRawFilePath

-- | Build `narinfo` suffix from `StorePath` which
-- can be used to query binary caches.
storePathToNarInfo :: StorePath -> Bytes.Char8.ByteString
storePathToNarInfo StorePath{..} =
  Text.encodeUtf8 $ encodeWith NixBase32 (coerce storePathHash) <> ".narinfo"

-- | Parse `StorePath` from `Bytes.Char8.ByteString`, checking
-- that store directory matches `expectedRoot`.
parsePath :: FilePath -> Bytes.Char8.ByteString -> Either String StorePath
parsePath expectedRoot x =
  let
    (rootDir, fname) = FilePath.splitFileName . Bytes.Char8.unpack $ x
    (storeBasedHashPart, namePart) = Text.breakOn "-" $ Text.pack fname
    storeHash = decodeWith NixBase32 storeBasedHashPart
    name = makeStorePathName . Text.drop 1 $ namePart
    --rootDir' = dropTrailingPathSeparator rootDir
    -- cannot use ^^ as it drops multiple slashes /a/b/// -> /a/b
    rootDir' = init rootDir
    storeDir =
      if expectedRoot == rootDir'
        then Right rootDir'
        else Left $ "Root store dir mismatch, expected" <> expectedRoot <> "got" <> rootDir'
  in
    StorePath <$> coerce storeHash <*> name <*> storeDir

pathParser :: FilePath -> Parser StorePath
pathParser expectedRoot = do
  _ <-
    Parser.Text.Lazy.string (Text.pack expectedRoot)
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

  either
    fail
    pure
    (StorePath <$> coerce digest <*> name <*> pure expectedRoot)
