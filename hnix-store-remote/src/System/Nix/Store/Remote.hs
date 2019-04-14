{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RecordWildCards     #-}
module System.Nix.Store.Remote (
    runStore
  , addTextToStore
  , syncWithGC
  , optimiseStore
  , verifyStore
  ) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Binary               as B
import qualified Data.Binary.Put           as B
import           Data.Maybe
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as M
import           Data.Proxy                (Proxy(Proxy))
import           Data.Text                 (Text)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T

import qualified System.Nix.Build          as Build
import qualified Nix.Derivation            as Drv

--import qualified System.Nix.GC             as GC
import           System.Nix.Hash           (Digest, ValidAlgo)
import           System.Nix.StorePath
import           System.Nix.Hash
import           System.Nix.Nar            (localPackNar, putNar, narEffectsIO, Nar)
import           System.Nix.Util
import           System.Nix.ValidPath

import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

import Data.Text.Encoding (encodeUtf8)

type RepairFlag = Bool
type CheckFlag = Bool
type CheckSigsFlag = Bool
type SubstituteFlag = Bool

--setOptions :: StoreSetting -> MonadStore ()

isValidPathUncached :: (KnownStoreDir a) => (StorePath a) -> MonadStore Bool
isValidPathUncached p = do
  simpleOpArgs IsValidPath $ putPath p

queryValidPaths :: (KnownStoreDir a) => (StorePathSet a) -> SubstituteFlag -> MonadStore (StorePathSet a)
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    putPaths ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: (KnownStoreDir a) => MonadStore (StorePathSet a)
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: (KnownStoreDir a) => (StorePathSet a) -> MonadStore (StorePathSet a)
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ do
    putPaths ps
  sockGetPaths

{-
querySubstitutablePathInfos :: (KnownStoreDir a) => (StorePathSet a) -> MonadStore [SubstitutablePathInfo]
querySubstitutablePathInfos ps = do
  runOpArgs QuerySubstitutablePathInfos $ do
    putPaths ps

  cnt <- sockGetInt
  forM (take cnt $ cycle [(0 :: Int)]) $ pure $ do
      _pth <- sockGetPath
      drv <- sockGetPath
      refs <- sockGetPaths
      dlSize <- sockGetInt
      narSize' <- sockGetInt
      return $ SubstitutablePathInfo {
                 deriver = drv
               , references = refs
               , downloadSize = dlSize
               , narSize = narSize'
               }
-}

queryPathInfoUncached :: (KnownStoreDir a) => (StorePath a) -> MonadStore (ValidPath a)
queryPathInfoUncached path = do
  runOpArgs QueryPathInfo $ do
    putPath path

  valid <- sockGetBool
  unless valid $ error "Path is not valid"

  deriver <- sockGetPath
  narHash <- lBSToText <$> sockGetStr
  references <- sockGetPaths
  registrationTime <- sockGet getTime
  narSize <- sockGetInt
  ultimate <- sockGetBool
  sigs <- map lBSToText <$> sockGetStrings
  ca <- lBSToText <$> sockGetStr
  return $ ValidPath {..}

queryReferrers :: (KnownStoreDir a) => (StorePath a) -> MonadStore (StorePathSet a)
queryReferrers p = do
  runOpArgs QueryReferrers $ do
    putPath p
  sockGetPaths

queryValidDerivers :: (KnownStoreDir a) => (StorePath a) -> MonadStore (StorePathSet a)
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ do
    putPath p
  sockGetPaths

queryDerivationOutputs :: (KnownStoreDir a) => (StorePath a) -> MonadStore (StorePathSet a)
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $
    putPath p
  sockGetPaths

queryDerivationOutputNames :: (KnownStoreDir a) => (StorePath a) -> MonadStore (StorePathSet a)
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $
    putPath p
  sockGetPaths

queryPathFromHashPart :: (KnownStoreDir a) => Digest StorePathHashAlgo -> MonadStore (Maybe (StorePath a))
queryPathFromHashPart storePathHash = do
  runOpArgs QueryPathFromHashPart $
    --putText $ printAsBase32 @PathHashAlgo digest
    putByteStringLen $ LBS.fromStrict $ encodeUtf8 $ encodeBase32 storePathHash
  sockGetPath

addToStoreNar :: (KnownStoreDir a) => ValidPath a -> Nar -> RepairFlag -> CheckSigsFlag -> MonadStore ()
addToStoreNar ValidPath{..} nar repair checkSigs = do
  void $ runOpArgs AddToStoreNar $ do
    putPath path
    maybe (return ()) (putPath) deriver
    putText narHash -- info.narHash.to_string(Base16, false)
    putPaths references
    putTime registrationTime
    putInt narSize
    putBool ultimate
    putTexts sigs
    putText ca

    -- << repair << !checkSigs;
    putBool repair
    putBool (not checkSigs)

  -- reference uses copyNAR here to just parse & dump existing NAR from path
  -- TUNNEL
  --  putNar nar

--type PathFilter = (StorePath a) -> Bool

addToStore
  :: forall p a. (KnownStoreDir p, ValidAlgo a, NamedAlgo a)
  => LBS.ByteString
  -> FilePath
  -> Bool
  -> Proxy a
  -> ((StorePath p) -> Bool)
  -> RepairFlag
  -> MonadStore (StorePath p)
addToStore name pth recursive algoProxy pfilter repair = do

  -- TODO: Is this lazy enough? We need `B.putLazyByteString bs` to stream `bs`
  bs  :: LBS.ByteString <- liftIO $ B.runPut . putNar <$> localPackNar narEffectsIO pth

  runOpArgs AddToStore $ do
    putByteStringLen name
    if algoName @a `elem` ["sha256"] && recursive -- , Truncated 20 SHA256] && recursive
      then putInt 0
      else putInt 1
    if recursive
      then putInt 1
      else putInt 0

    putByteStringLen (T.encodeUtf8 $ T.fromStrict $ algoName @a)

    B.putLazyByteString bs

  fmap (fromMaybe $ error "TODO: Error") sockGetPath

-- reference accepts repair but only uses it to throw error in case of nix daemon
addTextToStore :: (KnownStoreDir a)
               => Text
               -> Text
               -> (StorePathSet a)
               -> RepairFlag
               -> MonadStore (Maybe (StorePath a))
addTextToStore name text references' repair = do
  when repair $ error "repairing is not supported when building through the Nix daemon"
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths references'
  sockGetPath

buildPaths :: (KnownStoreDir a) => (StorePathSet a) -> Build.BuildMode -> MonadStore ()
buildPaths ps bm = do
  void $ simpleOpArgs BuildPaths $ do
    putPaths ps
    putInt $ fromEnum bm

{-
buildDerivation :: (KnownStoreDir a) => (StorePath a) -> Drv.Derivation -> Build.BuildMode -> MonadStore Build.BuildResult
buildDerivation p drv buildMode = do
  runOpArgs BuildDerivation $ do
    putPath p
    putDerivation drv
    putEnum buildMode
    putInt 0 -- ??????

  res <- getSocketIncremental $ getBuildResult
  return res
-}

ensurePath :: (KnownStoreDir a) => (StorePath a) -> MonadStore ()
ensurePath pn = do
  void $ simpleOpArgs EnsurePath $ putPath pn

addTempRoot :: (KnownStoreDir a) => (StorePath a) -> MonadStore ()
addTempRoot pn = do
  void $ simpleOpArgs AddTempRoot $ putPath pn

addIndirectRoot :: (KnownStoreDir a) => (StorePath a) -> MonadStore ()
addIndirectRoot pn = do
  void $ simpleOpArgs AddIndirectRoot $ putPath pn

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

{-
findRoots :: MonadStore Roots
findRoots = do
  runOp FindRoots
  res <- getSocketIncremental (do
      count <- getInt
      res <- sequence $ replicate count ((,) <$> getPath <*> getPath)
      return res
    )

  return $ M.fromList $ catMaybesTupled res
  where
   catMaybesTupled :: [(Maybe a, Maybe b)] -> [(a, b)]
   catMaybesTupled ls = map (\(Just x, Just y) -> (x, y)) $ filter (\(x,y) -> isJust x && isJust y) ls

collectGarbage :: GC.Options -> MonadStore GC.Result
collectGarbage opts = do
  runOpArgs CollectGarbage $ do
    putInt $ fromEnum $ GC.operation opts
    putPaths $ GC.pathsToDelete opts
    putBool $ GC.ignoreLiveness opts
    putInt $ GC.maxFreed opts
    -- removed options
    -- drop when collectGarbage drops these from nix/src/libstore/remote-store.cc
    forM_ [(0 :: Int)..2] $ pure $ putInt (0 :: Int)

  paths <- sockGetPaths
  freed <- sockGetInt
  _obsolete <- sockGetInt :: MonadStore Int

  return $ GC.Result paths freed
-}

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

queryMissing :: (KnownStoreDir a) => (StorePathSet a) -> MonadStore (StorePathSet a, StorePathSet a, StorePathSet a, Integer, Integer)
queryMissing ps = do
  runOpArgs QueryMissing $ do
    putPaths ps

  willBuild <- sockGetPaths
  willSubstitute <- sockGetPaths
  unknown <- sockGetPaths
  downloadSize' <- sockGetInt
  narSize' <- sockGetInt
  return (willBuild, willSubstitute, unknown, downloadSize', narSize')

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair

addSignatures :: (KnownStoreDir a) => (StorePath a) -> [LBS.ByteString] -> MonadStore ()
addSignatures p signatures = do
  void $ simpleOpArgs AddSignatures $ do
    putPath p
    putByteStrings signatures
