module Main where

import System.Nix.Store.DB.Run qualified

-- This only tests that database can be created
-- in-memory using migrateAll and that queryEverything
-- runs (with no data)
--
-- For better test, we would need a populated nix-store
main :: IO ()
main = System.Nix.Store.DB.Run.memTest
