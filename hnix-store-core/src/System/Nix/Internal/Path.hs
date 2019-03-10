{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}

{-|
Description : Internal path utilities
Maintainer  : srk <srk@48.io>
|-}
module System.Nix.Internal.Path where

import           Control.Monad
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Char8     as BSC
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           System.FilePath.Posix     (takeBaseName, takeDirectory)
import           System.Nix.Path           (Path(..), PathName(..), pathName, PathHashAlgo)
import           System.Nix.Internal.Hash  (Digest(..),  HashAlgorithm'( SHA256 ))
import           System.Nix.Hash           (HasDigest, printAsBase16, printAsBase32, printHashAlgo)
import qualified System.Nix.Hash

-- | Parse store location
parseStore :: BSL.ByteString -> T.Text
parseStore = T.pack . takeDirectory . BSC.unpack . BSL.toStrict

-- | Parse path from string
--
-- in form <storeDir>/<hash>-<pathName>
-- into (Just (Path <hash> (PathName <pathName>)))
-- or Nothing on error
--
-- XXX: should check for @PathHashAlgo length
parsePath :: BSL.ByteString -> Maybe Path
parsePath p = case name of
    Nothing -> Nothing
    Just n -> Just $ Path digest n
  where
  base = T.pack . takeBaseName . BSC.unpack . BSL.toStrict $ p
  parts = T.breakOn "-" base
  digest = Digest . BSC.pack . T.unpack . fst $ parts
  name = pathName . T.drop 1 . snd $ parts


-- experimental
-- Directory of the store
type StoreDir = Text
type Stored a = (StoreDir, a)

-- wrap StoreDir and Path into tuple
makeStored :: StoreDir -> Path -> Stored Path
makeStored sl p = (sl, p)

type PathType = Text
-- "text:<r1>:<r2>:...<rN>"
-- "source"
-- "output:<id>"
--    <id> is the name of the output (usually, "out").

-- store settings
data Settings = Settings {
  storeDir :: StoreDir -- settings.nixStore
  } deriving (Eq, Show)

-- build a store path in the following form:
-- <storeDir>/<hash>-<pathName>
storedToText :: Stored Path -> Text
storedToText (storeLoc, (Path digest pName)) = T.concat
  [ storeLoc
  , "/"
  , printAsBase32 @PathHashAlgo digest
  , "-"
  , pathNameContents pName
  ]

makeStorePath :: (HasDigest a) => PathType -> PathName -> Digest a -> Settings -> Text
makeStorePath typ pName digest settings = T.concat
  [ storeDir settings
  , "/"
  , printAsBase32 @PathHashAlgo $ pathHash typ pName digest (storeDir settings)
  , "-"
  , pathNameContents pName
  ]

makeStorePath' :: (HasDigest a) => PathType -> PathName -> Digest a -> StoreDir -> Path
makeStorePath' typ pName digest storeLoc = snd $ makeStoredPath typ pName digest storeLoc

-- | build Stored Path from the type of the path, path name and a digest stored at StoreDir
-- As StoreDir is part of the path hashing process we need to take it into account
-- when building Path(s)
makeStoredPath :: (HasDigest a) => PathType -> PathName -> Digest a -> StoreDir -> Stored Path
makeStoredPath typ pName digest storeLoc = makeStored storeLoc $ Path (pathHash typ pName digest storeLoc) pName

-- build <h> string which is a truncated base32 formatted SHA256 hash of <s>
pathHash :: (HasDigest a) => PathType -> PathName -> Digest a -> StoreDir -> Digest PathHashAlgo
pathHash typ pName digest storeLoc = System.Nix.Hash.hash . BSC.pack . T.unpack $
  makePathDigestString typ pName digest storeLoc

-- build <s> string which is hashed and used in makeStorePath
-- <s> = "<pathType>:<hash_algo>:<base16_hash>:<storeDir>:<pathName>"
-- (exposed for testing purposes only)
makePathDigestString ::  (HasDigest a) => PathType -> PathName -> Digest a -> StoreDir -> Text
makePathDigestString typ pName digest storeLoc = T.intercalate (T.pack ":")
  [ typ
  , printHashAlgo digest
  , printAsBase16 digest
  , storeLoc
  , pathNameContents pName
  ]

-- make output path from `PathName` digest and outputId which typically is "out"
makeOutputPath :: (HasDigest a) => PathName -> Digest a -> T.Text -> Settings -> Text
makeOutputPath pName digest outputId settings =
  makeStorePath typ (adjustName pName) digest settings
  where
    typ = T.concat [ "output:", outputId ]
    adjustName n | outputId == "out" = n
    adjustName (PathName name) | otherwise = PathName $ T.concat [ name, T.pack "-", outputId ]

type Recursive = Bool
-- make fixed output path from `PathName` and Recursive option
makeFixedOutputPath :: (HasDigest a) => PathName -> Digest a -> Recursive -> Settings -> Text
makeFixedOutputPath pName digest True      settings =  -- XXX: this needs be restricted to @a == @SHA256
  makeStorePath ("source") pName digest settings
makeFixedOutputPath pName digest recursive settings =
  makeStorePath ("output:out") pName digest' settings
  where
    rec True = "r:"
    rec False = T.empty
    digest' = System.Nix.Hash.hash @SHA256 $ BSC.pack . T.unpack . T.concat $
      [ "fixed:out:"
      , rec recursive
      , printHashAlgo digest
      , printAsBase16 digest
      , ":"
      ]

-- references should be PathSet not [T.Text]
-- but how to turn PathSet into store paths (texts) again
-- when we don't have PathType
type References = [T.Text]

makeTextPath :: (HasDigest a) => PathName -> Digest a -> References-> Settings -> Text
makeTextPath pName digest references settings =
  makeStorePath typ pName digest settings
  where typ = T.concat $ [ "text" ] ++ (map (T.cons ':') references)

storePathForText :: PathName -> T.Text -> References -> Settings -> Text
storePathForText pName contents references settings =
  makeTextPath pName hashOfContents references settings
  where hashOfContents = System.Nix.Hash.hash @SHA256 (BSC.pack . T.unpack $ contents)
