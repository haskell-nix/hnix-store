{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Nix.ReadonlyStore where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import           System.Nix.Hash
import           System.Nix.StorePath

makeOutputPath :: forall storeDir hashAlgo . (KnownStoreDir storeDir, NamedAlgo hashAlgo) => Text -> Digest hashAlgo -> StorePathName -> Maybe (StorePath storeDir)
makeOutputPath id h nm = makeStorePath ty h <$> name
  where
    ty = BS.intercalate ":" ["output", encodeUtf8 id]
    name = if id == "out" then Just nm else
      makeStorePathName $ T.concat [unStorePathName nm, "-", id]

makeStorePath :: forall storeDir hashAlgo . (KnownStoreDir storeDir, NamedAlgo hashAlgo) => ByteString -> Digest hashAlgo -> StorePathName -> StorePath storeDir
makeStorePath ty h nm = StorePath storeHash nm
  where
    s = BS.intercalate ":"
      [ ty
      , encodeUtf8 $ algoName @hashAlgo
      , encodeUtf8 $ encodeBase16 h
      , storeDirVal @storeDir
      , encodeUtf8 $ unStorePathName nm
      ]
    storeHash = hash s

makeType :: (KnownStoreDir storeDir) => ByteString -> StorePathSet storeDir -> ByteString
makeType name refs = BS.intercalate ":" (name : map storePathToRawFilePath (HS.toList refs))

makeTextPath :: (KnownStoreDir storeDir) => StorePathName -> Digest 'SHA256 -> StorePathSet storeDir -> StorePath storeDir
makeTextPath nm h refs = makeStorePath (makeType "text" refs) h nm

makeFixedOutputPath :: forall hashType storeDir. (KnownStoreDir storeDir, NamedAlgo hashType, ValidAlgo hashType)
    => Bool -> Digest hashType -> StorePathName -> StorePathSet storeDir -> StorePath storeDir
makeFixedOutputPath recursive digest name refs =
    if recursive && algoName @hashType == "sha256"
    then makeStorePath (makeType "source" refs) digest name
    else if HS.null refs
      then makeStorePath "output:out" fixedDigest name
      else error "Old style fixed output path cannot have references"
  where
    fixedDigest = hash @'SHA256 $ BS.concat 
        [ "fixed:out:"
        , (if recursive then "r:" else "") 
        , encodeUtf8 (encodeBase16 digest)
        , ":"
        ]

computeStorePathForText :: (KnownStoreDir storeDir) => StorePathName -> ByteString -> StorePathSet storeDir -> StorePath storeDir
computeStorePathForText nm s refs = makeTextPath nm (hash s) refs

data DerivationOutput storeDir = DerivationOutput
    { path :: StorePath storeDir
    , drvHash :: Text
    , hashAlgo :: HashAlgorithm
    }

data Derivation storeDir = Derivation
    { name :: StorePathName
    , outputs :: M.Map Text (StorePath storeDir)
    , inputSrcs :: HS.HashSet (StorePath storeDir)
    , inputDrvs :: M.Map (StorePath storeDir) [Text]
    , platform :: Text
    , builder :: Text -- should be typed as a store path
    , args :: [ Text ]
    , env :: M.Map Text Text
    }

