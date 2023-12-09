{-# LANGUAGE OverloadedStrings #-}

module NixDaemonSpec
  ( enterNamespaces
  , spec
  ) where

import Control.Monad (forM_, unless, void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (SHA256)
import Data.Some (Some(Some))
import Data.Text (Text)
import Test.Hspec (Spec, SpecWith, around, describe, context)
import Test.Hspec.Expectations.Lifted
import Test.Hspec.Nix (forceRight)
import System.FilePath ((</>))
import System.Linux.Namespaces (Namespace(..), GroupMapping(..), UserMapping(..))
import System.Nix.Hash (HashAlgo(HashAlgo_SHA256))
import System.Nix.Build (BuildMode(..))
import System.Nix.DerivedPath (DerivedPath(..))
import System.Nix.StorePath (StoreDir(..), StorePath)
import System.Nix.StorePath.Metadata (Metadata(..))
import System.Nix.Store.Remote
import System.Process (CreateProcess(..), ProcessHandle)
import qualified Control.Concurrent
import qualified Control.Exception
import qualified Data.ByteString.Char8
import qualified Data.Either
import qualified Data.HashSet
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified System.Directory
import qualified System.Environment
import qualified System.IO.Temp
import qualified System.Linux.Namespaces
import qualified System.Nix.StorePath
import qualified System.Nix.Nar
import qualified System.Posix.User
import qualified System.Process
import qualified Test.Hspec

createProcessEnv
  :: FilePath
  -> String
  -> [String]
  -> IO ProcessHandle
createProcessEnv fp proc args = do
  mPath         <- System.Environment.lookupEnv "PATH"

  (_, _, _, ph) <-
    System.Process.createProcess (System.Process.proc proc args)
      { cwd = Just fp
      , env = Just $ mockedEnv mPath fp
      }
  pure ph

mockedEnv
  :: Maybe String
  -> FilePath
  -> [(String, FilePath)]
mockedEnv mEnvPath fp =
  [ ("NIX_STORE_DIR"     , fp </> "store")
  , ("NIX_LOCALSTATE_DIR", fp </> "var")
  , ("NIX_LOG_DIR"       , fp </> "var" </> "log")
  , ("NIX_STATE_DIR"     , fp </> "var" </> "nix")
  , ("NIX_CONF_DIR"      , fp </> "etc")
  , ("HOME"              , fp </> "home")
--  , ("NIX_REMOTE", "daemon")
    ] <> foldMap (\x -> [("PATH", x)]) mEnvPath

waitSocket
  :: FilePath
  -> Int
  -> IO ()
waitSocket _  0 = fail "No socket"
waitSocket fp x = do
  ex <- System.Directory.doesFileExist fp
  unless ex $ do
    Control.Concurrent.threadDelay 100000
    waitSocket fp (x - 1)

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
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> IO (ProcessHandle, RemoteStoreT m a -> Run m a)
startDaemon fp = do
  writeConf (fp </> "etc" </> "nix.conf")
  procHandle <- createProcessEnv fp "nix-daemon" []
  waitSocket sockFp 30
  pure ( procHandle
       , runStoreConnection (StoreConnection_Socket (StoreSocketPath sockFp))
         . (setStoreDir (StoreDir $ Data.ByteString.Char8.pack $ fp </> "store")
            >>
           )
       )
 where
  sockFp = fp </> "var/nix/daemon-socket/socket"

enterNamespaces :: IO ()
enterNamespaces = do
  uid <- System.Posix.User.getEffectiveUserID
  gid <- System.Posix.User.getEffectiveGroupID

  System.Linux.Namespaces.unshare
    [User, Network, Mount]

  -- fmap our (parent) uid to root
  System.Linux.Namespaces.writeUserMappings
    Nothing
    [ UserMapping
        0    -- inside namespace
        uid  -- outside namespace
        1    --range
    ]

  -- fmap our (parent) gid to root group
  System.Linux.Namespaces.writeGroupMappings
    Nothing
    [ GroupMapping 0 gid 1 ]
    True

withNixDaemon
  :: ( MonadIO m
     , MonadMask m
     )
  => ((RemoteStoreT m a -> Run m a) -> IO a)
  -> IO a
withNixDaemon action =
  System.IO.Temp.withSystemTempDirectory "test-nix-store" $ \path -> do

    mapM_ (System.Directory.createDirectory . snd)
          (filter
            ((/= "NIX_REMOTE") . fst)
            $ mockedEnv Nothing path)

    ini <- createProcessEnv path "nix-store" ["--init"]
    void $ System.Process.waitForProcess ini

    writeFile (path </> "dummy") "Hello World"

    System.Directory.setCurrentDirectory path

    Control.Exception.bracket
      (startDaemon path)
      (System.Process.terminateProcess . fst)
      (action . snd)

checks
  :: ( Show a
     , Show b
     )
  => IO (a, b)
  -> (a -> Bool)
  -> IO ()
checks action check =
  action >>= (`Test.Hspec.shouldSatisfy` (check . fst))

it
  :: (Show a, Show b, Monad m)
  => String
  -> m c
  -> (a -> Bool)
  -> SpecWith (m () -> IO (a, b))
it name action check =
  Test.Hspec.it name $ \run -> run (void $ action) `checks` check

itRights
  :: ( Show a
     , Show b
     , Show c
     , Monad m
     )
  => String
  -> m d
  -> SpecWith (m () -> IO (Either a b, c))
itRights name action = it name action Data.Either.isRight

itLefts
  :: ( Show a
     , Show b
     , Show c
     , Monad m
     )
  => String
  -> m d
  -> SpecWith (m () -> IO (Either a b, c))
itLefts name action = it name action Data.Either.isLeft

withPath
  :: MonadRemoteStore m
  => (StorePath -> m a)
  -> m a
withPath action = do
  path <-
    addTextToStore
      (StoreText
        (forceRight $ System.Nix.StorePath.mkStorePathName "hnix-store")
        "test"
      )
      mempty
      RepairMode_DontRepair
  action path

-- | dummy path, adds <tmp>/dummy with "Hello World" contents
dummy :: MonadRemoteStore m => m StorePath
dummy = do
  addToStore
    (forceRight $ System.Nix.StorePath.mkStorePathName "dummy")
    (System.Nix.Nar.dumpPath "dummy")
    FileIngestionMethod_Flat
    (Some HashAlgo_SHA256)
    RepairMode_DontRepair

invalidPath :: StorePath
invalidPath =
  let name = forceRight $ System.Nix.StorePath.mkStorePathName "invalid"
  in System.Nix.StorePath.unsafeMakeStorePath
      (System.Nix.StorePath.mkStorePathHashPart
        @SHA256
        "invalid")
      name

_withBuilder
  :: MonadRemoteStore m
  => (StorePath -> m a)
  -> m a
_withBuilder action = do
  path <-
    addTextToStore
      (StoreText (forceRight $ System.Nix.StorePath.mkStorePathName "builder") builderSh)
      mempty
      RepairMode_DontRepair
  action path

builderSh :: Text
builderSh = "declare -xpexport > $out"

spec :: Spec
spec = around withNixDaemon $

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

    context "isValidPath" $ do
      itRights "validates path" $ withPath $ \path -> do
        liftIO $ print path
        isValidPath path `shouldReturn` True
      itLefts "fails on invalid path" $ do
        setStoreDir (StoreDir "/asdf")
        isValidPath invalidPath

    context "queryAllValidPaths" $ do
      itRights "empty query" queryAllValidPaths
      itRights "non-empty query" $ withPath $ \path ->
        queryAllValidPaths `shouldReturn` Data.HashSet.fromList [path]

    context "queryPathInfo" $
      itRights "queries path info" $ withPath $ \path -> do
        meta <- queryPathInfo path
        (metadataReferences <$> meta) `shouldBe` (Just mempty)

    context "ensurePath" $
      itRights "simple ensure" $ withPath ensurePath

    context "addTempRoot" $
      itRights "simple addition" $ withPath addTempRoot

    context "addIndirectRoot" $
      itRights "simple addition" $ withPath addIndirectRoot

    let toDerivedPathSet p = Data.Set.fromList [DerivedPath_Opaque p]

    context "buildPaths" $ do
      itRights "build Normal" $ withPath $ \path -> do
        buildPaths (toDerivedPathSet path) BuildMode_Normal

      itRights "build Check" $ withPath $ \path -> do
        buildPaths (toDerivedPathSet path) BuildMode_Check

      itLefts "build Repair" $ withPath $ \path -> do
        buildPaths (toDerivedPathSet path) BuildMode_Repair

    context "roots" $ context "findRoots" $ do
        itRights "empty roots" (findRoots `shouldReturn` mempty)

        itRights "path added as a temp root" $ withPath $ \_ -> do
          roots <- findRoots
          roots `shouldSatisfy` ((== 1) . Data.Map.size)

    context "optimiseStore" $ itRights "optimises" optimiseStore

    context "queryMissing" $
      itRights "queries" $ withPath $ \path -> do
        queryMissing (toDerivedPathSet path)
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
        fp <-
          liftIO
          $ System.IO.Temp.writeSystemTempFile
              "addition"
              "yolo"

        addToStore
          (forceRight $ System.Nix.StorePath.mkStorePathName "tmp-addition")
          (System.Nix.Nar.dumpPath fp)
          FileIngestionMethod_Flat
          (Some HashAlgo_SHA256)
          RepairMode_DontRepair

    context "with dummy" $ do
      itRights "adds dummy" dummy

      itRights "valid dummy" $ do
        path <- dummy
        isValidPath path `shouldReturn` True

    context "collectGarbage" $ do
      itRights "delete a specific path from the store" $ withPath $ \path -> do
          -- clear temp gc roots so the delete works. restarting the nix daemon should also do this...
          storeDir <- getStoreDir
          let tempRootsDir = Data.Text.unpack $ mconcat [ Data.Text.Encoding.decodeUtf8 (unStoreDir storeDir), "/../var/nix/temproots/" ]
          tempRootList <-
            liftIO
            $ System.Directory.listDirectory
                tempRootsDir
          liftIO $ forM_ tempRootList $ \entry -> do
            System.Directory.removeFile
              $ mconcat [ tempRootsDir, "/", entry ]

          GCResult{..} <-
            collectGarbage
              GCOptions
                { gcOptionsOperation = GCAction_DeleteSpecific
                , gcOptionsIgnoreLiveness = False
                , gcOptionsPathsToDelete = Data.HashSet.fromList [path]
                , gcOptionsMaxFreed = maxBound
                }
          gcResultDeletedPaths `shouldBe` Data.HashSet.fromList [path]
          gcResultBytesFreed `shouldBe` 4
