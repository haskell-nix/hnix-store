{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module NixDaemon where

import           Prelude
import           Control.Monad
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Exception           (bracket)
import           Control.Concurrent          (threadDelay)
import           Data.Either                 (isRight, isLeft, fromRight)
import           Data.Binary.Put
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import           Data.Time
import qualified Data.Text                   as T
import qualified Data.Text.Lazy.Builder
import qualified Data.HashSet                as HS
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import           System.Directory
import qualified System.Environment
import           System.IO.Temp
import qualified System.IO                   as IO (hGetContents, hPutStr, openFile)
import qualified System.Process              as P
import           System.Posix.User           as U
import           System.Linux.Namespaces     as NS
import           Test.Tasty                  as T
import           Test.Tasty.Hspec            (Spec, HasCallStack, describe, context)
import qualified Test.Tasty.Hspec            as Hspec
import           Test.Hspec.Expectations.Lifted
import qualified Test.Tasty.HUnit            as HU
import           Test.Tasty.QuickCheck
import           Text.Read                   (readMaybe)

import           System.FilePath

import           System.Nix.Build
import           System.Nix.Hash
import           System.Nix.StorePath
import           System.Nix.ReadonlyStore
import           System.Nix.Nar
import qualified System.Nix.StorePathMetadata as VP
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

import           Derivation

createProcessEnv :: FilePath
                 -> String
                 -> [String]
                 -> IO P.ProcessHandle
createProcessEnv fp proc args = do
  mPath <- System.Environment.lookupEnv "PATH"

  (_, _, _, ph) <- P.createProcess (P.proc proc args) { P.cwd = Just $ fp
                                                      , P.env = Just $ mockedEnv mPath fp }
  return ph

mockedEnv :: Maybe String -> FilePath -> [(String, FilePath)]
mockedEnv mEnvPath fp = map (\(a, b) -> (a, b)) [
    ("NIX_STORE_DIR", fp </> "store")
  , ("NIX_LOCALSTATE_DIR", fp </> "var")
  , ("NIX_LOG_DIR", fp </> "var" </> "log")
  , ("NIX_STATE_DIR", fp </> "var" </> "nix")
  , ("NIX_CONF_DIR", fp </> "etc")
--  , ("NIX_REMOTE", "daemon")
  ] ++ (maybe [] (\x -> [("PATH", x)]) mEnvPath)

waitSocket :: FilePath -> Int -> IO ()
waitSocket fp 0 = fail "No socket"
waitSocket fp x = do
  ex <- doesFileExist fp
  case ex of
    True -> return ()
    False -> threadDelay 100000 >> waitSocket fp (x - 1)

writeConf fp = do
  writeFile fp $ unlines [
      "build-users-group = "
    , "trusted-users = root"
    , "allowed-users = *"
    , "fsync-metadata = false"
    ]

{-
 - we run in user namespace as root but groups are failed
 - => build-users-group has to be empty but we still
 - get an error (maybe older nix-daemon)
 -
uid=0(root) gid=65534(nobody) groups=65534(nobody)

drwxr-xr-x 3 0 65534 60 Nov 29 05:53 store

accepted connection from pid 22959, user root (trusted)
error: changing ownership of path '/run/user/1000/test-nix-store-06b0d249e5616122/store': Invalid argument
-}

startDaemon :: FilePath -> IO (P.ProcessHandle, MonadStore a -> IO (Either String a, [Logger]))
startDaemon fp = do
  writeConf (fp </> "etc" </> "nix.conf")
  p <- createProcessEnv fp "nix-daemon" []
  waitSocket sockFp 30
  return (p, runStoreOpts sockFp (fp </> "store"))
  where
    sockFp = fp </> "var/nix/daemon-socket/socket"

enterNamespaces = do
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID

  unshare [User, Network, Mount]
  -- map our (parent) uid to root
  writeUserMappings Nothing [UserMapping 0 uid 1]
  -- map our (parent) gid to root group
  writeGroupMappings Nothing [GroupMapping 0 gid 1] True

withNixDaemon action = do
  withSystemTempDirectory "test-nix-store" $ \path -> do

    mapM_ (createDirectory . snd)
      (filter ((/= "NIX_REMOTE") . fst) $ mockedEnv Nothing path)

    ini <- createProcessEnv path
      "nix-store" ["--init"]
    P.waitForProcess ini

    writeFile (path </> "dummy") "Hello World"

    setCurrentDirectory path

    bracket (startDaemon path)
            (P.terminateProcess . fst)
            (\x -> action . snd $ x)

checks action check = action >>= (`Hspec.shouldSatisfy` (check . fst))
it name action check = Hspec.it name $ \run -> (run (action >> return ())) `checks` check
itRights name action = it name action isRight
itLefts name action = it name action isLeft

withPath action = do
  path <- addTextToStore "hnix-store" "test" (HS.fromList []) False
  action path

-- | dummy path, adds <tmp>/dummpy with "Hello World" contents
dummy = do
  let Right n = makeStorePathName "dummy"
  res <- addToStore @SHA256 n "dummy" False (pure True) False
  return res

invalidPath :: StorePath
invalidPath =
  let Right n = makeStorePathName "invalid"
  in  StorePath (hash "invalid") n "no_such_root"

withBuilder action = do
  path <- addTextToStore "builder" builderSh (HS.fromList []) False
  action path

builderSh = T.concat [ "declare -xp", "export > $out" ]

spec_protocol :: Spec
spec_protocol = Hspec.around withNixDaemon $ do

  describe "store" $ do

    context "syncWithGC" $ do
      itRights "syncs with garbage collector" syncWithGC

    context "verifyStore" $ do
      itRights "check=False repair=False" $ do
        verifyStore False False `shouldReturn` False

      itRights "check=True repair=False" $ do
        verifyStore True False `shouldReturn` False

      --privileged
      itRights "check=True repair=True" $ do
        verifyStore True True `shouldReturn` False

    context "addTextToStore" $ do
      itRights "adds text to store" $ withPath $ const return ()

    context "isValidPathUncached" $ do
      itRights "validates path" $ withPath $ \path -> do
        liftIO $ putStrLn $ show path
        (isValidPathUncached path) `shouldReturn` True
      itLefts "fails on invalid path" $ isValidPathUncached $ invalidPath

    context "queryAllValidPaths" $ do
      itRights "empty query" $ queryAllValidPaths
      itRights "non-empty query" $ withPath $ \path -> queryAllValidPaths `shouldReturn` (HS.fromList [path])

    context "queryPathInfoUncached" $ do
      itRights "queries path info" $ withPath $ queryPathInfoUncached @SHA256

    context "ensurePath" $ do
      itRights "simple ensure" $ withPath $ ensurePath

    context "addTempRoot" $ do
      itRights "simple addition" $ withPath $ addTempRoot

    context "addIndirectRoot" $ do
      itRights "simple addition" $ withPath $ addIndirectRoot

    context "buildPaths" $ do
      itRights "build Normal" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet Normal

      itRights "build Check" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet Check

      itLefts "build Repair" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet Repair

    context "roots" $ do
      context "findRoots" $ do
        itRights "empty roots" $ (findRoots `shouldReturn` M.empty)

        itRights "path added as a temp root" $ withPath $ \path -> do
          roots <- findRoots
          roots `shouldSatisfy` ((==1) . M.size)

    context "optimiseStore" $ do
      itRights "optimises" $ optimiseStore

    context "queryMissing" $ do
      itRights "queries" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        queryMissing pathSet `shouldReturn` (HS.empty, HS.empty, HS.empty, 0, 0)

    context "addToStore" $ do
      itRights "adds file to store" $ do
        fp <- liftIO $ writeSystemTempFile "addition" "lal"
        let Right n = makeStorePathName "tmp-addition"
        res <- addToStore @SHA256 n fp False (pure True) False
        liftIO $ print res

    context "with dummy" $ do
      itRights "adds dummy" dummy

      itRights "valid dummy" $ do
        path <- dummy
        liftIO $ putStrLn $ show path
        (isValidPathUncached path) `shouldReturn` True

    context "derivation" $ do
      itRights "build derivation" $ do
        withDerivation $ \path drv -> do
          result <- buildDerivation path drv Normal
          result `shouldSatisfy` ((==AlreadyValid) . status)
