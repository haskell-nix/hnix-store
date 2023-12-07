module System.Nix.Store.Remote.Types.StoreReply
  ( StoreReply(..)
  ) where

import System.Nix.Build (BuildResult)
import System.Nix.StorePath (HasStoreDir(..), StorePath)
import System.Nix.Store.Remote.Serializer
import System.Nix.Store.Remote.Types.GC (GCResult)
import System.Nix.Store.Remote.Types.ProtoVersion (HasProtoVersion)

-- | Get @NixSerializer@ for some type @a@
-- This could also be generalized for every type
-- we have a serializer for but we mostly need
-- this for replies and it would make look serializers
-- quite hodor, like @a <- getS get; b <- getS get@
class StoreReply a where
  getReplyS
    :: ( HasStoreDir r
       , HasProtoVersion r
       )
    => NixSerializer r ReplySError a

instance StoreReply Bool where
  getReplyS = mapPrimE bool

instance StoreReply BuildResult where
  getReplyS = buildResult

instance StoreReply GCResult where
  getReplyS = gcResult

instance StoreReply StorePath where
  getReplyS = mapPrimE storePath

mapPrimE
  :: NixSerializer r SError a
  -> NixSerializer r ReplySError a
mapPrimE = mapErrorS ReplySError_PrimGet
