module Main where

import System.FilePath ((</>))
import System.Process (readProcess, system)
import System.IO.Temp (withSystemTempDirectory)
import System.SetEnv (setEnv, unsetEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)

import Nix.LocalStore (allocateLocalStore)
import Nix.Store (isValidPath)

main :: IO ()
main = withSystemTempDirectory "nixtest-XXX" $ \dir -> do
    setEnv "NIX_STORE_DIR" $ dir </> "store"
    setEnv "NIX_LOCALSTATE_DIR" $ dir </> "var"
    setEnv "NIX_LOG_DIR" $ dir </> "var/log/nix"
    setEnv "NIX_STATE_DIR" $ dir </> "var/nix"
    setEnv "NIX_CONF_DIR" $ dir </> "etc"
    unsetEnv "NIX_REMOTE"
    unsetEnv "NIX_BUILD_HOOK"
    _ <- system "nix-store --init"
    path <- readProcess "nix-store" [ "--add", "./Test.hs" ] "" >>= return . init
    runResourceT $ do
        (_, store) <- allocateLocalStore $ dir </> "var/nix/db/db.sqlite"
        liftIO $ isValidPath store path >>= putStrLn . show
        _ <- liftIO . system $ "nix-store --delete " ++ path
        liftIO $ isValidPath store path >>= putStrLn . show
