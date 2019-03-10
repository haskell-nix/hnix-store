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
  , isValidPathUncached
  , queryValidPaths
  , queryAllValidPaths
  , querySubstitutablePaths
  , querySubstitutablePathInfos
  , queryPathInfoUncached
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryDerivationOutputNames
  , queryPathFromHashPart
  , addToStoreNar
  , addToStore
  , addTextToStore
  , buildPaths
  , buildDerivation
  , ensurePath
  , addTempRoot
  , addIndirectRoot
  , syncWithGC
  , findRoots
  , collectGarbage
  , optimiseStore
  , verifyStore
  , addSignatures
  , queryMissing
  ) where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Binary               as B
import qualified Data.Binary.Put           as B
import           Data.Maybe
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Map.Strict           as M
import           Data.Proxy                (Proxy(Proxy))
import           Data.Text                 (Text)
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T

import qualified System.Nix.Build          as Build
import qualified Nix.Derivation            as Drv

import qualified System.Nix.GC             as GC
import           System.Nix.Hash           (Digest, HashAlgorithm)
import           System.Nix.Path
import           System.Nix.Hash
import           System.Nix.Nar            (localPackNar, putNar, narEffectsIO, Nar)
import           System.Nix.Util
import           System.Nix.ValidPath

import           System.Nix.Store.Remote.Types
import           System.Nix.Store.Remote.Protocol
import           System.Nix.Store.Remote.Util

-- tmp
import qualified Data.ByteString.Base64.Lazy as B64

type RepairFlag = Bool
type CheckFlag = Bool
type CheckSigsFlag = Bool
type SubstituteFlag = Bool

--setOptions :: StoreSetting -> MonadStore ()

isValidPathUncached :: Path -> MonadStore Bool
isValidPathUncached p = do
  sd <- getStoreDir
  simpleOpArgs IsValidPath $ putPath sd p

queryValidPaths :: PathSet -> SubstituteFlag -> MonadStore PathSet
queryValidPaths ps substitute = do
  sd <- getStoreDir
  runOpArgs QueryValidPaths $ do
    putPaths sd ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: MonadStore PathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: PathSet -> MonadStore PathSet
querySubstitutablePaths ps = do
  sd <- getStoreDir
  runOpArgs QuerySubstitutablePaths $ do
    putPaths sd ps
  sockGetPaths

querySubstitutablePathInfos :: PathSet -> MonadStore [SubstitutablePathInfo]
querySubstitutablePathInfos ps = do
  sd <- getStoreDir
  runOpArgs QuerySubstitutablePathInfos $ do
    putPaths sd ps

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

queryPathInfoUncached :: Path -> MonadStore ValidPath
queryPathInfoUncached path = do
  sd <- getStoreDir
  runOpArgs QueryPathInfo $ do
    putPath sd path

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

queryReferrers :: Path -> MonadStore PathSet
queryReferrers p = do
  sd <- getStoreDir
  runOpArgs QueryReferrers $ do
    putPath sd p
  sockGetPaths

queryValidDerivers :: Path -> MonadStore PathSet
queryValidDerivers p = do
  sd <- getStoreDir
  runOpArgs QueryValidDerivers $ do
    putPath sd p
  sockGetPaths

queryDerivationOutputs :: Path -> MonadStore PathSet
queryDerivationOutputs p = do
  sd <- getStoreDir
  runOpArgs QueryDerivationOutputs $
    putPath sd p
  sockGetPaths

queryDerivationOutputNames :: Path -> MonadStore PathSet
queryDerivationOutputNames p = do
  sd <- getStoreDir
  runOpArgs QueryDerivationOutputNames $
    putPath sd p
  sockGetPaths

queryPathFromHashPart :: Digest PathHashAlgo -> MonadStore (Maybe Path)
queryPathFromHashPart digest = do
  runOpArgs QueryPathFromHashPart $
    putText $ printAsBase32 @PathHashAlgo digest
  sockGetPath

addToStoreNar :: ValidPath -> Nar -> RepairFlag -> CheckSigsFlag -> MonadStore ()
addToStoreNar ValidPath{..} nar repair checkSigs = do
  sd <- getStoreDir
  void $ runOpArgs AddToStoreNar $ do
    putPath sd path
    maybe (return ()) (putPath sd) deriver
    putText narHash -- info.narHash.to_string(Base16, false)
    putPaths sd references
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

printHashType :: HashAlgorithm' Integer -> T.Text
printHashType MD5             = "MD5"
printHashType SHA1            = "SHA1"
printHashType SHA256          = "SHA256"
printHashType (Truncated _ a) = printHashType a

type PathFilter = Path -> Bool

addToStore
  :: forall a. (HasDigest a, AlgoVal a)
  => LBS.ByteString
  -> FilePath
  -> Bool
  -> Proxy a
  -> PathFilter
  -> RepairFlag
  -> MonadStore Path
addToStore name pth recursive algoProxy pfilter repair = do

  -- TODO: Is this lazy enough? We need `B.putLazyByteString bs` to stream `bs`
  bs  :: LBS.ByteString <- liftIO $ B.runPut . putNar <$> localPackNar narEffectsIO pth

  runOpArgs AddToStore $ do
    putByteStringLen name
    if algoVal @a `elem` [SHA256, Truncated 20 SHA256] && recursive
      then putInt 0
      else putInt 1
    if recursive
      then putInt 1
      else putInt 0

    putByteStringLen (T.encodeUtf8 . T.toLower . printHashType $ algoVal @a)

    B.putLazyByteString bs

  fmap (fromMaybe $ error "TODO: Error") sockGetPath

-- reference accepts repair but only uses it to throw error in case of nix daemon
addTextToStore :: Text -> Text -> PathSet -> RepairFlag -> MonadStore (Maybe Path)
addTextToStore name text references' repair = do
  when repair $ error "repairing is not supported when building through the Nix daemon"
  sd <- getStoreDir
  runOpArgs AddTextToStore $ do
    putText name
    putText text
    putPaths sd references'
  sockGetPath

buildPaths :: PathSet -> Build.BuildMode -> MonadStore ()
buildPaths ps bm = do
  sd <- getStoreDir
  void $ simpleOpArgs BuildPaths $ do
    putPaths sd ps
    putInt $ fromEnum bm

buildDerivation :: Path -> Drv.Derivation -> Build.BuildMode -> MonadStore Build.BuildResult
buildDerivation p drv buildMode = do
  sd <- getStoreDir
  runOpArgs BuildDerivation $ do
    putPath sd p
    putDerivation drv
    putEnum buildMode
    putInt 0 -- ??????

  res <- getSocketIncremental $ getBuildResult
  return res

ensurePath :: Path -> MonadStore ()
ensurePath pn = do
  sd <- getStoreDir
  void $ simpleOpArgs EnsurePath $ putPath sd pn

addTempRoot :: Path -> MonadStore ()
addTempRoot pn = do
  sd <- getStoreDir
  void $ simpleOpArgs AddTempRoot $ putPath sd pn

addIndirectRoot :: Path -> MonadStore ()
addIndirectRoot pn = do
  sd <- getStoreDir
  void $ simpleOpArgs AddIndirectRoot $ putPath sd pn

syncWithGC :: MonadStore ()
syncWithGC = void $ simpleOp SyncWithGC

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
  sd <- getStoreDir
  runOpArgs CollectGarbage $ do
    putInt $ fromEnum $ GC.operation opts
    putPaths sd $ GC.pathsToDelete opts
    putBool $ GC.ignoreLiveness opts
    putInt $ GC.maxFreed opts
    -- removed options
    -- drop when collectGarbage drops these from nix/src/libstore/remote-store.cc
    forM_ [(0 :: Int)..2] $ pure $ putInt (0 :: Int)

  paths <- sockGetPaths
  freed <- sockGetInt
  _obsolete <- sockGetInt :: MonadStore Int

  return $ GC.Result paths freed

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

queryMissing :: PathSet -> MonadStore (PathSet, PathSet, PathSet, Integer, Integer)
queryMissing ps = do
  sd <- getStoreDir
  runOpArgs QueryMissing $ do
    putPaths sd ps

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

addSignatures :: Path -> [LBS.ByteString] -> MonadStore ()
addSignatures p signatures = do
  sd <- getStoreDir
  void $ simpleOpArgs AddSignatures $ do
    putPath sd p
    putByteStrings signatures
