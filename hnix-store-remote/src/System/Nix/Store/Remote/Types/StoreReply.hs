module System.Nix.Store.Remote.Types.StoreReply
  ( StoreReply(..)
  ) where

import Data.HashSet (HashSet)
import Data.Map (Map)
import System.Nix.Build (BuildResult)
import System.Nix.StorePath (StorePath, StorePathName)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.NoReply (NoReply(..))
import System.Nix.Store.Remote.Types.SuccessCodeReply (SuccessCodeReply)
import System.Nix.Store.Remote.Types.GC (GCResult, GCRoot)
import System.Nix.Store.Remote.Types.Query.Missing (Missing)
import System.Nix.Store.Remote.Types.StoreConfig (ProtoStoreConfig)

-- | Get @NixSerializer@ for some type @a@
-- This could also be generalized for every type
-- we have a serializer for but we mostly need
-- this for replies and it would make look serializers
-- quite hodor, like @a <- getS get; b <- getS get@
class StoreReply a where
  getReplyS :: NixSerializer ProtoStoreConfig ReplySError a

instance StoreReply SuccessCodeReply where
  getReplyS = opSuccess

instance StoreReply NoReply where
  getReplyS = noop NoReply

instance StoreReply Bool where
  getReplyS = mapPrimE bool

instance StoreReply BuildResult where
  getReplyS = buildResult

instance StoreReply GCResult where
  getReplyS = gcResult

instance StoreReply (Map GCRoot StorePath) where
  getReplyS = mapS gcRoot (mapPrimE storePath)

instance StoreReply Missing where
  getReplyS = missing

instance StoreReply (Maybe (Metadata StorePath)) where
  getReplyS = maybePathMetadata

instance StoreReply StorePath where
  getReplyS = mapPrimE storePath

instance StoreReply (HashSet StorePath) where
  getReplyS = mapPrimE (hashSet storePath)

instance StoreReply (HashSet StorePathName) where
  getReplyS = mapPrimE (hashSet storePathName)

mapPrimE
  :: NixSerializer r SError a
  -> NixSerializer r ReplySError a
mapPrimE = mapErrorS ReplySError_PrimGet
