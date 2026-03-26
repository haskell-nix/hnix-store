module System.Nix.Store.Remote.Types.StoreReply
  ( StoreReply(..)
  ) where

import Data.HashSet (HashSet)
import Data.Map (Map)
import System.Nix.Build (BuildResult)
import System.Nix.StorePath (StoreDir, StorePath, StorePathName)
import System.Nix.StorePath.Metadata (Metadata)
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.NoReply (NoReply(..))
import System.Nix.Store.Remote.Types.SuccessCodeReply (SuccessCodeReply)
import System.Nix.Store.Remote.Types.GC (GCResult, GCRoot)
import System.Nix.Store.Remote.Types.ProtoVersion (ProtoVersion)
import System.Nix.Store.Remote.Types.Query.Missing (Missing)

-- | Get @NixSerializer@ for some type @a@
-- This could also be generalized for every type
-- we have a serializer for but we mostly need
-- this for replies and it would make look serializers
-- quite hodor, like @a <- getS get; b <- getS get@
class StoreReply a where
  getReplyS :: StoreDir -> ProtoVersion -> NixSerializer ReplySError a

instance StoreReply SuccessCodeReply where
  getReplyS _ _ = opSuccess

instance StoreReply NoReply where
  getReplyS _ _ = noop NoReply

instance StoreReply Bool where
  getReplyS _ _ = mapPrimE bool

instance StoreReply BuildResult where
  getReplyS sd pv = buildResult sd pv

instance StoreReply GCResult where
  getReplyS sd _ = gcResult sd

instance StoreReply (Map GCRoot StorePath) where
  getReplyS sd _ = mapS gcRoot (mapPrimE (storePath sd))

instance StoreReply Missing where
  getReplyS sd _ = missing sd

instance StoreReply (Maybe (Metadata StorePath)) where
  getReplyS sd _ = maybePathMetadata sd

instance StoreReply StorePath where
  getReplyS sd _ = mapPrimE (storePath sd)

instance StoreReply (HashSet StorePath) where
  getReplyS sd _ = mapPrimE (hashSet (storePath sd))

instance StoreReply (HashSet StorePathName) where
  getReplyS _ _ = mapPrimE (hashSet storePathName)

mapPrimE
  :: NixSerializer SError a
  -> NixSerializer ReplySError a
mapPrimE = mapErrorS ReplySError_PrimGet
