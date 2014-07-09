module Nix.RemoteStore (
  RemoteStore
, allocateRemoteStore
) where

import qualified Network.Socket as S
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import Nix.Settings (Settings(..))
import Nix.Internal.ResourceSocket (ResourceSocket, allocateResourceSocket, connect)

data RemoteStore = RemoteStore ResourceSocket

allocateRemoteStore :: MonadResource m
                    => Settings
                    -> m (ReleaseKey, RemoteStore)
allocateRemoteStore settings = do
    (key, s) <- allocateResourceSocket S.AF_UNIX S.Stream S.defaultProtocol
    liftIO $ connect s (S.SockAddrUnix $ daemonSocketFile settings)
    -- TODO: greeting, versioning
    return (key, RemoteStore s)
