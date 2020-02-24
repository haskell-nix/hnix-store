{-|
Description : Interact with a local binary Nix store.
Maintainer  : Brian McKenna <brian@brianmckenna.org>
-}
module System.Nix.Store.Binary.Local (
  localBinaryStoreEffects
) where

import Control.Monad.Trans (MonadIO(..))
import System.Directory
import System.FilePath
import System.Nix.Store.Binary
import qualified Data.ByteString as BS
import qualified Data.Text as T

localBinaryStoreEffects
  :: (MonadIO m)
  => FilePath
  -> BinaryStoreEffects m
localBinaryStoreEffects cacheDir =
  BinaryStoreEffects upsertFile'
  where
    upsertFile' pth d _ = liftIO $ do
      let pth' = cacheDir </> T.unpack pth
      createDirectoryIfMissing True $ takeDirectory pth'
      BS.writeFile pth' d
