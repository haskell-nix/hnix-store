{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where

import Control.Monad (forM_, replicateM_, unless)
import Data.ByteString qualified as Bytes
import Data.IORef (modifyIORef', newIORef, readIORef)
import System.Directory qualified as Directory
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Printf (printf)
import Text.Read (readMaybe)

import System.Nix.Nar.Streamer (dumpPath)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["prepare", fixturePath, fileCountText] -> do
      fileCount <- readPositive "file count" fileCountText
      prepareFixture fixturePath fileCount
    ["run", fixturePath, iterationsText] -> do
      iterations <- readPositive "iteration count" iterationsText
      runBenchmark fixturePath iterations
    [] -> putStrLn usage
    _ -> die usage

usage :: String
usage = unlines
  [ "Usage: nar-stream prepare FIXTURE_PATH FILE_COUNT"
  , "       nar-stream run FIXTURE_PATH ITERATIONS"
  ]

readPositive :: String -> String -> IO Int
readPositive label input =
  case readMaybe input of
    Just value | value > 0 -> pure value
    _ -> die $ label <> " must be a positive integer"

prepareFixture :: FilePath -> Int -> IO ()
prepareFixture fixturePath fileCount = do
  exists <- Directory.doesPathExist fixturePath
  if exists
    then die $ "fixture path already exists: " <> fixturePath
    else Directory.createDirectory fixturePath

  forM_ [1 .. fileCount] $ \index ->
    Bytes.writeFile (fixturePath </> fileName index) Bytes.empty
 where
  fileName :: Int -> FilePath
  fileName = printf "file-%08d"

runBenchmark :: FilePath -> Int -> IO ()
runBenchmark fixturePath iterations = do
  exists <- Directory.doesDirectoryExist fixturePath
  unless exists $ die $ "fixture directory does not exist: " <> fixturePath

  byteCount <- newIORef (0 :: Int)
  replicateM_ iterations $
    dumpPath fixturePath $ \chunk ->
      modifyIORef' byteCount (+ Bytes.length chunk)

  streamedBytes <- readIORef byteCount
  unless (streamedBytes > 0) $ die "NAR streamer produced no output"
