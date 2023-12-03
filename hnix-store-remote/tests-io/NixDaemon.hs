{-# LANGUAGE OverloadedStrings #-}

module NixDaemon where

import Data.Text (Text)
import Data.Either (isRight, isLeft)
import Data.Bool (bool)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified System.Environment
import Control.Exception (bracket)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Either
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text
import qualified Data.Text.Encoding
import System.Directory
import System.IO.Temp
import qualified System.Process as P
import System.Posix.User as U
import System.Linux.Namespaces as NS
import Test.Hspec (Spec, describe, context)
import qualified Test.Hspec as Hspec
import Test.Hspec.Expectations.Lifted
import System.FilePath
import System.Nix.Build
import System.Nix.StorePath
import System.Nix.StorePath.Metadata
import System.Nix.Store.Remote
import System.Nix.Store.Remote.Client (Run)
import System.Nix.Store.Remote.MonadStore (mapStoreConfig)

import Crypto.Hash (SHA256)
import System.Nix.Nar (dumpPath)

createProcessEnv :: FilePath -> String -> [String] -> IO P.ProcessHandle
createProcessEnv fp proc args = do
  mPath         <- System.Environment.lookupEnv "PATH"

  (_, _, _, ph) <-
    P.createProcess (P.proc proc args)
      { P.cwd = Just fp
      , P.env = Just $ mockedEnv mPath fp
      }
  pure ph

mockedEnv :: Maybe String -> FilePath -> [(String, FilePath)]
mockedEnv mEnvPath fp =
  [ ("NIX_STORE_DIR"     , fp </> "store")
  , ("NIX_LOCALSTATE_DIR", fp </> "var")
  , ("NIX_LOG_DIR"       , fp </> "var" </> "log")
  , ("NIX_STATE_DIR"     , fp </> "var" </> "nix")
  , ("NIX_CONF_DIR"      , fp </> "etc")
  , ("HOME"              , fp </> "home")
--  , ("NIX_REMOTE", "daemon")
    ] <> foldMap (\x -> [("PATH", x)]) mEnvPath

waitSocket :: FilePath -> Int -> IO ()
waitSocket _  0 = fail "No socket"
waitSocket fp x = do
  ex <- doesFileExist fp
  bool
    (threadDelay 100000 >> waitSocket fp (x - 1))
    (pure ())
    ex

writeConf :: FilePath -> IO ()
writeConf fp =
  writeFile fp $ unlines
    [ "build-users-group = "
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

startDaemon
  :: FilePath
  -> IO (P.ProcessHandle, MonadStore a -> Run IO a)
startDaemon fp = do
  writeConf (fp </> "etc" </> "nix.conf")
  p <- createProcessEnv fp "nix-daemon" []
  waitSocket sockFp 30
  pure (p, runStoreOpts sockFp (StoreDir $ BSC.pack $ fp </> "store"))
 where
  sockFp = fp </> "var/nix/daemon-socket/socket"

enterNamespaces :: IO ()
enterNamespaces = do
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID

  unshare [User, Network, Mount]
  -- fmap our (parent) uid to root
  writeUserMappings Nothing [UserMapping 0 uid 1]
  -- fmap our (parent) gid to root group
  writeGroupMappings Nothing [GroupMapping 0 gid 1] True

withNixDaemon
  :: ((MonadStore a -> Run IO a) -> IO a) -> IO a
withNixDaemon action =
  withSystemTempDirectory "test-nix-store" $ \path -> do

    mapM_ (createDirectory . snd)
          (filter ((/= "NIX_REMOTE") . fst) $ mockedEnv Nothing path)

    ini <- createProcessEnv path "nix-store" ["--init"]
    void $ P.waitForProcess ini

    writeFile (path </> "dummy") "Hello World"

    setCurrentDirectory path

    bracket (startDaemon path)
            (P.terminateProcess . fst)
            (action . snd)

checks :: (Show a, Show b) => IO (a, b) -> (a -> Bool) -> IO ()
checks action check = action >>= (`Hspec.shouldSatisfy` (check . fst))

it
  :: (Show a, Show b, Monad m)
  => String
  -> m c
  -> (a -> Bool)
  -> Hspec.SpecWith (m () -> IO (a, b))
it name action check =
  Hspec.it name $ \run -> run (void $ action) `checks` check

itRights
  :: (Show a, Show b, Show c, Monad m)
  => String
  -> m d
  -> Hspec.SpecWith (m () -> IO (Either a b, c))
itRights name action = it name action isRight

itLefts
  :: (Show a, Show b, Show c, Monad m)
  => String
  -> m d
  -> Hspec.SpecWith (m () -> IO (Either a b, c))
itLefts name action = it name action isLeft

withPath :: (StorePath -> MonadStore a) -> MonadStore a
withPath action = do
  path <- addTextToStore "hnix-store" "test" mempty RepairMode_DontRepair
  action path

-- | dummy path, adds <tmp>/dummy with "Hello World" contents
dummy :: MonadStore StorePath
dummy = do
  let name = Data.Either.fromRight (error "impossible") $ makeStorePathName "dummy"
  addToStore @SHA256 name (dumpPath "dummy") FileIngestionMethod_Flat RepairMode_DontRepair

invalidPath :: StorePath
invalidPath =
  let name = Data.Either.fromRight (error "impossible") $ makeStorePathName "invalid"
  in  unsafeMakeStorePath (mkStorePathHashPart @SHA256 "invalid") name

withBuilder :: (StorePath -> MonadStore a) -> MonadStore a
withBuilder action = do
  path <- addTextToStore "builder" builderSh mempty RepairMode_DontRepair
  action path

builderSh :: Text
builderSh = "declare -xpexport > $out"

spec_protocol :: Spec
spec_protocol = Hspec.around withNixDaemon $

  describe "store" $ do

    context "syncWithGC" $
      itRights "syncs with garbage collector" syncWithGC

    context "verifyStore" $ do
      itRights "check=False repair=False" $
        verifyStore
          CheckMode_DontCheck
          RepairMode_DontRepair
        `shouldReturn` False

      itRights "check=True repair=False" $
        verifyStore
          CheckMode_DoCheck
          RepairMode_DontRepair
        `shouldReturn` False

      --privileged
      itRights "check=True repair=True" $
        verifyStore
          CheckMode_DoCheck
          RepairMode_DoRepair
        `shouldReturn` False

    context "addTextToStore" $
      itRights "adds text to store" $ withPath pure

    context "isValidPathUncached" $ do
      itRights "validates path" $ withPath $ \path -> do
        liftIO $ print path
        isValidPathUncached path `shouldReturn` True
      itLefts "fails on invalid path"
        $ mapStoreConfig
            (\sc -> sc { storeConfig_dir = StoreDir "/asdf" })
            $ isValidPathUncached invalidPath

    context "queryAllValidPaths" $ do
      itRights "empty query" queryAllValidPaths
      itRights "non-empty query" $ withPath $ \path ->
        queryAllValidPaths `shouldReturn` HS.fromList [path]

    context "queryPathInfoUncached" $
      itRights "queries path info" $ withPath $ \path -> do
        meta <- queryPathInfoUncached path
        references meta `shouldSatisfy` HS.null

    context "ensurePath" $
      itRights "simple ensure" $ withPath ensurePath

    context "addTempRoot" $
      itRights "simple addition" $ withPath addTempRoot

    context "addIndirectRoot" $
      itRights "simple addition" $ withPath addIndirectRoot

    context "buildPaths" $ do
      itRights "build Normal" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet BuildMode_Normal

      itRights "build Check" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet BuildMode_Check

      itLefts "build Repair" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        buildPaths pathSet BuildMode_Repair

    context "roots" $ context "findRoots" $ do
        itRights "empty roots" (findRoots `shouldReturn` M.empty)

        itRights "path added as a temp root" $ withPath $ \_ -> do
          roots <- findRoots
          roots `shouldSatisfy` ((== 1) . M.size)

    context "optimiseStore" $ itRights "optimises" optimiseStore

    context "queryMissing" $
      itRights "queries" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        queryMissing pathSet
        `shouldReturn`
        Missing
          { missingWillBuild = mempty
          , missingWillSubstitute = mempty
          , missingUnknownPaths = mempty
          , missingDownloadSize = 0
          , missingNarSize = 0
          }

    context "addToStore" $
      itRights "adds file to store" $ do
        fp <- liftIO $ writeSystemTempFile "addition" "lal"
        let name = Data.Either.fromRight (error "impossible") $ makeStorePathName "tmp-addition"
        res <- addToStore @SHA256 name (dumpPath fp) FileIngestionMethod_Flat RepairMode_DontRepair
        liftIO $ print res

    context "with dummy" $ do
      itRights "adds dummy" dummy

      itRights "valid dummy" $ do
        path <- dummy
        liftIO $ print path
        isValidPathUncached path `shouldReturn` True

    context "deleteSpecific" $
      itRights "delete a path from the store" $ withPath $ \path -> do
          -- clear temp gc roots so the delete works. restarting the nix daemon should also do this...
          storeDir <- getStoreDir
          let tempRootsDir = Data.Text.unpack $ mconcat [ Data.Text.Encoding.decodeUtf8 (unStoreDir storeDir), "/../var/nix/temproots/" ]
          tempRootList <- liftIO $ listDirectory tempRootsDir
          liftIO $ forM_ tempRootList $ \entry -> do
            removeFile $ mconcat [ tempRootsDir, "/", entry ]

          GCResult{..} <- deleteSpecific (HS.fromList [path])
          gcResult_deletedPaths `shouldBe` HS.fromList [path]
          gcResult_bytesFreed `shouldBe` 4

