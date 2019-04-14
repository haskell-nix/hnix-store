{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module NixDaemon where

import           Prelude                     hiding (FilePath)
import           Control.Monad
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import           Control.Exception           (bracket)
import           Control.Concurrent          (threadDelay)
import           Data.Either                 (isRight, isLeft)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BSL
import           Data.Monoid                 ((<>))
import           Data.Maybe                  (fromJust)
import           Data.Time
import qualified Data.Text                   as T
import qualified Data.Text.Lazy.Builder
import qualified Data.HashSet                as HS
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Vector                 as V
import           System.Directory            --(doesFileExist, createDirectory, copyFile)
import           System.IO.Temp              -- (withSystemTempDirectory, writeSystemTempFile, createTempDirectory)
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

import           Filesystem.Path
import           Filesystem.Path.CurrentOS
import           System.Nix.Build
import           System.Nix.Hash
import           System.Nix.StorePath
import           System.Nix.Nar
import qualified System.Nix.ValidPath        as VP
import           System.Nix.Store.Remote
import           System.Nix.Store.Remote.Logger
import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util
--import qualified System.Nix.GC               as GC

import           Data.Proxy

-- derivation parsing
import qualified Nix.Derivation as Drv
import qualified Data.Text.Lazy              as TL
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Attoparsec.Text.Lazy as A

createProcessEnv :: FilePath -> String -> [String] -> IO P.ProcessHandle
createProcessEnv fp proc args =do
  (_, _, _, ph) <- P.createProcess (P.proc proc args) { P.cwd = Just $ encodeString fp
                                                      , P.env = Just $ mockedEnv fp }
  return ph

mockedEnv :: FilePath -> [(String, String)]
mockedEnv fp = map (\(a, b) -> (a, encodeString b)) [
    ("NIX_STORE_DIR", fp </> "store")
  , ("NIX_LOCALSTATE_DIR", fp </> "var")
  , ("NIX_LOG_DIR", fp </> "var" </> "log")
  , ("NIX_STATE_DIR", fp </> "var" </> "nix")
  , ("NIX_CONF_DIR", fp </> "etc")
--  , ("NIX_REMOTE", "daemon")
  ]

waitSocket :: FilePath -> Int -> IO ()
waitSocket fp 0 = fail "No socket"
waitSocket fp x = do
  ex <- doesFileExist (encodeString fp)
  case ex of
    True -> return ()
    False -> threadDelay 100000 >> waitSocket fp (x - 1)

writeConf fp = do
  TIO.writeFile fp $ TL.unlines [
      "build-users-group = "
    , "trusted-users = root"
    , "allowed-users = *"
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
  writeConf (encodeString $ fp </> "etc" </> "nix.conf")
  p <- createProcessEnv fp "nix-daemon" []
  waitSocket sockFp 30
  return (p, runStoreOpts (encodeString sockFp) (encodeString (fp </> "store")))
  where
    sockFp = fp </> "var/nix/daemon-socket/socket"

enterNamespaces = do
  uid <- getEffectiveUserID
  unshare [User, Network, Mount]
  writeUserMappings Nothing [UserMapping 0 uid 1]
  -- permission denied :(
  --writeGroupMappings Nothing [GroupMapping 65534 ? 1] True

withNixDaemon action = do
  withSystemTempDirectory "test-nix-store" $ \pth -> do
    let path = decodeString pth -- oh my

    mapM_ (createDirectory . snd) (filter ((/= "NIX_REMOTE") . fst) $ mockedEnv path)

    ini <- createProcessEnv path "nix-store" ["--init"]
    P.waitForProcess ini

    bracket (startDaemon path)
            (P.terminateProcess . fst)
            (action . snd)

checks action check = action >>= (`Hspec.shouldSatisfy` (check . fst))
it name action check = Hspec.it name $ \run -> (run (action >> return ())) `checks` check
itRights name action = it name action isRight
itLefts name action = it name action isLeft

withPath action = do
  (Just path) <- addTextToStore "hnix-store" "test" (HS.fromList [])  False
  action path

invalidPath = StorePath (hash "invalid") $ fromJust $ makeStorePathName "invalid"
{-
 - broken

withDrv action = withBuilder $ \builder -> withBash $ \bash -> do
  path <- addTextToStore "wannabe-output" "" (HS.fromList [])  False

  let unPath (Path digest pname) = pathNameContents pname
      d = drvSample (unPath bash) (fromText $ unPath builder) (decodeString $ ((T.unpack $ unPath path) ++ "-out"))

  (Just path) <- addTextToStore "hnix-store-derivation" (
    TL.toStrict $ Data.Text.Lazy.Builder.toLazyText $ Drv.buildDerivation d)
    (HS.fromList []) False
  liftIO $ print d
  action path d
-}

{-
lal = do
  --fp <- fmap init <$> liftIO $ P.readProcess "which" ["bash"] ""
  --parent <- liftIO getCanonicalTemporaryDirectory
  --pth <- liftIO $ createTempDirectory parent "test-nix-store-import"
  --liftIO $ copyFile fp (pth ++ "/bash")
  nar <- liftIO $ localPackNar narEffectsIO "src"
  now <- liftIO $ getCurrentTime

  -- makeOutputPath "myout" digest "out" (StoreLocation)
  (Just path) <- addTextToStore "wannabe-output" "" (HS.fromList [])  False
  let unPath (Path digest pname) = pathNameContents pname

  let vp = VP.ValidPath
            { VP.path =  path
            , VP.deriver = Nothing
            , VP.narHash = (printAsBase32 @PathHashAlgo (hash "dunno"))
            , VP.references = HS.empty
            , VP.registrationTime = now
            , VP.narSize = 100
            , VP.ultimate = True
            , VP.sigs = []
            , VP.ca = ""
            }

  addToStoreNar vp nar False False

  --liftIO $ removeDirectoryRecursive pth

-}
{-
withBash action = do
  fp <- fmap init <$> liftIO $ P.readProcess "which" ["bash"] ""
  path <- addToStore "bash" fp False (Proxy :: Proxy 'SHA256) (pure True) False
  action path
-}

withBuilder action = do
  (Just path) <- addTextToStore "builder" builderSh (HS.fromList []) False
  action path

builderSh = T.concat [ "declare -xp", "export > $out" ]

drvSample builder buildScript out = Drv.Derivation {
    Drv.outputs = M.fromList [ ("out", Drv.DerivationOutput out "sha256" "lal") ]
  , Drv.inputDrvs = M.empty -- Map FilePath (Set Text)
  , Drv.inputSrcs = S.fromList [ buildScript ]
  , Drv.platform = "x86_64-linux"
  , Drv.builder = builder
  , Drv.args = V.fromList ["-e", printFP buildScript ]
--  , Drv.env = M.empty
  , Drv.env = M.fromList [("testEnv", "true")]
  }

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

{-
    context "addTextToStore" $ do
      itRights "adds text to store" $ withPath $ const return ()
    context "isValidPathUncached" $ do
      itRights "validates path" $ withPath $ \path -> do
        (isValidPathUncached path) `shouldReturn` True
      itLefts "fails on invalid path" $ isValidPathUncached $ invalidPath

    context "queryAllValidPaths" $ do
      itRights "empty query" $ queryAllValidPaths
      itRights "non-empty query" $ withPath $ \path -> queryAllValidPaths `shouldReturn` (HS.fromList [path])

    context "queryPathInfoUncached" $ do
      itRights "queries path info" $ withPath $ queryPathInfoUncached
-}
    {-
    context "ensurePath" $ do
      itRights "simple ensure" $ withPath $ ensurePath

    context "addTempRoot" $ do
      itRights "simple addition" $ withPath $ addTempRoot

    context "addIndirectRoot" $ do
      itRights "simple addition" $ withPath $ addIndirectRoot

    context "collectGarbage" $ do
      itRights "simple collect nothing" $ do
        gc <- collectGarbage $ GC.Options
          { GC.operation = GC.DeleteDead
          , GC.pathsToDelete = HS.empty
          , GC.ignoreLiveness = False
          , GC.maxFreed = -1 }

        gc `shouldBe` (GC.Result {GC.paths = HS.empty, GC.bytesFreed = 0})

      itLefts "cannot gargabe collect live path" $ withPath $ \path -> do
        ensurePath path

        collectGarbage $ GC.Options
          { GC.operation = GC.DeleteSpecific
          , GC.pathsToDelete = HS.fromList [path]
          , GC.ignoreLiveness = False
          , GC.maxFreed = -1 }
    -}

    {-
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

    -}

    {-
    context "optimiseStore" $ do
      itRights "optimises" $ optimiseStore

    context "queryMissing" $ do
      itRights "queries" $ withPath $ \path -> do
        let pathSet = HS.fromList [path]
        queryMissing pathSet `shouldReturn` (HS.empty, HS.empty, HS.empty, 0, 0)
    -}

    {-
    context "addToStore" $ do
      itRights "adds file to store" $ do
        fp <- liftIO $ writeSystemTempFile "addition" "lal"
        res <- addToStore "test" fp False SHA256 (pure True) False
        liftIO $ print res

      itRights "adds bash to store" $ withBash $ const return ()

    itRights "build derivation" $ do
      withDrv $ \path drv -> do
        res <- buildDerivation path drv Normal
        res `shouldSatisfy` ((==TransientFailure) . status)
        liftIO $ print res

        --liftIO $ forever $ threadDelay 1000000

        return ()

    itRights "matches paths" $ do
      (Just path) <- addTextToStore "lal" "Hello World" (HS.fromList [])  False
      liftIO $ print path
      return ()
      -- /nix/store/fnxqxrjyhksy7x3ilzz9lixrynwcnz3q-lal


    -}
      {-
    itRights "nars" $ lal
      drv <- liftIO $ do
        text <- TIO.readFile drvFP'
        case A.parse Drv.parseDerivation text of
            A.Fail _uncomsumed _contexts error -> fail $ "Derivation parsing error " ++ error
            A.Done _unconsumed result -> return result
      -}


{-
pec_lol :: Spec
pec_lol = around withNixDaemon $ do
  describe "store" $ do
    it "addsText" $ \run -> do
      (run $ addTextToStore "hnix-store" "test" (HS.fromList [])  False)  `checks` ()
-}
