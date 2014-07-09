module Nix.RemoteStore
    ( -- * The @RemoteStore@ type
      RemoteStore
    , allocateRemoteStore
    ) where

import qualified Network.Socket as S
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Resource
    ( MonadResource
    , ReleaseKey
    )

import Nix.Settings
    ( Settings(..)
    )
import Nix.Internal.ResourceSocket
    ( ResourceSocket
    , allocateResourceSocket
    , connect
    )

-- | Implementation of the nix store API using nix-daemon socket protocol.
data RemoteStore = RemoteStore !ResourceSocket

-- | Set up a connected @RemoteStore@ within a @MonadResource@, registering a
-- cleanup action to tear down the connection.
allocateRemoteStore :: MonadResource m
                    => Settings  -- ^ The settings for the store connection
                    -> m (ReleaseKey, RemoteStore)
allocateRemoteStore settings = do
    (key, s) <- allocateResourceSocket S.AF_UNIX S.Stream S.defaultProtocol
    liftIO $ connect s (S.SockAddrUnix $ daemonSocketFile settings)
    -- TODO: greeting, versioning
    return (key, RemoteStore s)
