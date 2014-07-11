module Main where

import System.FilePath ((</>))
import System.Process (readProcess, system)
import System.IO.Temp (withSystemTempDirectory)
import System.SetEnv (setEnv, unsetEnv)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashSet as HS

import Nix.LocalStore (allocateLocalStore)
import Nix.Store (isValidPath, queryValidPaths)

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
    path1 <- readProcess "nix-store" [ "--add", "./Test.hs" ] "" >>= return . init
    path2 <- readProcess "nix-store" [ "--add", "./Setup.hs" ] "" >>= return . init
    runResourceT $ do
        (_, store) <- allocateLocalStore $ dir </> "var/nix/db/db.sqlite"
        liftIO $ isValidPath store path1 >>= putStrLn . show
        liftIO $ queryValidPaths store (HS.fromList [ path1, path2 ]) >>= putStrLn . show
        _ <- liftIO . system $ "nix-store --delete " ++ path1
        liftIO $ isValidPath store path1 >>= putStrLn . show
        liftIO $ queryValidPaths store (HS.fromList [ path1, path2 ]) >>= putStrLn . show
