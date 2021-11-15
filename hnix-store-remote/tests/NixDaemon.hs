{-# language DataKinds           #-}
{-# language ScopedTypeVariables #-}

module NixDaemon where

import qualified System.Environment            as Env
import           Control.Exception              ( bracket )
import           Control.Concurrent             ( threadDelay )
import qualified Data.HashSet                  as HS
import qualified Data.Map.Strict               as M
import           System.Directory
import           System.IO.Temp
import qualified System.Process                as P
import           System.Posix.User             as U
import           System.Linux.Namespaces       as NS
import           Test.Hspec                     ( Spec
                                                , describe
                                                , context
                                                )
import qualified Test.Hspec                    as Hspec
import           Test.Hspec.Expectations.Lifted

import           System.FilePath

import           System.Nix.Build
import           System.Nix.StorePath
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Protocol

import           Derivation
import           Crypto.Hash                    ( SHA256
                                                )

createProcessEnv :: FilePath -> String -> [String] -> IO P.ProcessHandle
createProcessEnv fp proc args = do
  mPath         <- Env.lookupEnv "PATH"

  (_, _, _, ph) <-
    P.createProcess (P.proc proc args)
      { P.cwd = Just fp
      , P.env = Just $ mockedEnv mPath fp
      }
  pure ph

mockedEnv :: Maybe String -> FilePath -> [(String, FilePath)]
mockedEnv mEnvPath fp = (fp </>) <<$>>
  [ ("NIX_STORE_DIR"     , "store")
  , ("NIX_LOCALSTATE_DIR", "var")
  , ("NIX_LOG_DIR"       , "var" </> "log")
  , ("NIX_STATE_DIR"     , "var" </> "nix")
  , ("NIX_CONF_DIR"      , "etc")
--  , ("NIX_REMOTE", "daemon")
    ] <> foldMap (\x -> [("PATH", x)]) mEnvPath

waitSocket :: FilePath -> Int -> IO ()
waitSocket _  0 = fail "No socket"
waitSocket fp x = do
  ex <- doesFileExist fp
  bool
    (threadDelay 100000 >> waitSocket fp (x - 1))
    pass
    ex

writeConf :: FilePath -> IO ()
writeConf fp =
  writeFileText fp $ unlines
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
  -> IO (P.ProcessHandle, MonadStore a -> IO (Either String a, [Logger]))
startDaemon fp = do
  writeConf (fp </> "etc" </> "nix.conf")
  p <- createProcessEnv fp "nix-daemon" []
  waitSocket sockFp 30
  pure (p, runStoreOpts sockFp (fp </> "store"))
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
  :: ((MonadStore a -> IO (Either String a, [Logger])) -> IO a) -> IO a
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
  Hspec.it name $ \run -> run (action >> pass) `checks` check

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
  path <- addTextToStore "hnix-store" "test" (HS.fromList []) False
  action path

-- | dummy path, adds <tmp>/dummpy with "Hello World" contents
dummy :: MonadStore StorePath
dummy = do
  let Right n = makeStorePathName "dummy"
  addToStore @SHA256 n "dummy" False (pure True) False

invalidPath :: StorePath
invalidPath =
  let Right n = makeStorePathName "invalid"
  in  StorePath (mkStorePathHashPart "invalid") n "no_such_root"

withBuilder :: (StorePath -> MonadStore a) -> MonadStore a
withBuilder action = do
  path <- addTextToStore "builder" builderSh (HS.fromList []) False
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
        verifyStore False False `shouldReturn` False

      itRights "check=True repair=False" $
        verifyStore True False `shouldReturn` False

      --privileged
      itRights "check=True repair=True" $
        verifyStore True True `shouldReturn` False

    context "addTextToStore" $
      itRights "adds text to store" $ withPath pure

    context "isValidPathUncached" $ do
      itRights "validates path" $ withPath $ \path -> do
        liftIO $ print path
        isValidPathUncached path `shouldReturn` True
      itLefts "fails on invalid path" $ isValidPathUncached invalidPath

    context "queryAllValidPaths" $ do
      itRights "empty query" queryAllValidPaths
      itRights "non-empty query" $ withPath $ \path ->
        queryAllValidPaths `shouldReturn` HS.fromList [path]

    context "queryPathInfoUncached" $
      itRights "queries path info" $ withPath queryPathInfoUncached

    context "ensurePath" $
      itRights "simple ensure" $ withPath ensurePath

    context "addTempRoot" $
      itRights "simple addition" $ withPath addTempRoot

    context "addIndirectRoot" $
      itRights "simple addition" $ withPath addIndirectRoot

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

    context "roots" $ context "findRoots" $ do
        itRights "empty roots" (findRoots `shouldReturn` M.empty)

        itRights "path added as a temp root" $ withPath $ \_ -> do
          roots <- findRoots
          roots `shouldSatisfy` ((== 1) . M.size)

    context "optimiseStore" $ itRights "optimises" optimiseStore

    context "queryMissing" $
      itRights "queries" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        queryMissing pathSet `shouldReturn` (HS.empty, HS.empty, HS.empty, 0, 0)

    context "addToStore" $
      itRights "adds file to store" $ do
        fp <- liftIO $ writeSystemTempFile "addition" "lal"
        let Right n = makeStorePathName "tmp-addition"
        res <- addToStore @SHA256 n fp False (pure True) False
        liftIO $ print res

    context "with dummy" $ do
      itRights "adds dummy" dummy

      itRights "valid dummy" $ do
        path <- dummy
        liftIO $ print path
        isValidPathUncached path `shouldReturn` True

    context "derivation" $
      itRights "build derivation" $
        withDerivation $ \path drv -> do
          result <- buildDerivation path drv Normal
          result `shouldSatisfy` ((== AlreadyValid) . status)
