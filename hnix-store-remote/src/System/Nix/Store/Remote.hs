{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
import qualified Data.Text.Lazy                 as T
import qualified Data.Text.Lazy.Encoding        as T

import qualified System.Nix.Build          as Build
import qualified System.Nix.Derivation     as Drv
import qualified System.Nix.GC             as GC
import           System.Nix.Hash           (Digest, HashAlgorithm)
import           System.Nix.Path
import           System.Nix.Hash
import           System.Nix.Nar            (localPackNar, putNar, narEffectsIO)
import           System.Nix.Util

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
isValidPathUncached p = simpleOpArgs IsValidPath $ putPath p

queryValidPaths :: PathSet -> SubstituteFlag -> MonadStore PathSet
queryValidPaths ps substitute = do
  runOpArgs QueryValidPaths $ do
    putPaths ps
    putBool substitute
  sockGetPaths

queryAllValidPaths :: MonadStore PathSet
queryAllValidPaths = do
  runOp QueryAllValidPaths
  sockGetPaths

querySubstitutablePaths :: PathSet -> MonadStore PathSet
querySubstitutablePaths ps = do
  runOpArgs QuerySubstitutablePaths $ do
    putPaths ps
  sockGetPaths

querySubstitutablePathInfos :: PathSet -> MonadStore [SubstitutablePathInfo]
querySubstitutablePathInfos ps = do
  runOpArgs QuerySubstitutablePathInfos $ do
    putPaths ps

  cnt <- sockGetInt
  forM (take cnt $ cycle [(0 :: Int)]) $ pure $ do
      _pth <- sockGetPath
      drv <- sockGetStr
      refs <- sockGetPaths
      dlSize <- sockGetInt
      narSize' <- sockGetInt
      return $ SubstitutablePathInfo {
                 deriver = mkPath drv
               , references = refs
               , downloadSize = dlSize
               , narSize = narSize'
               }

queryPathInfoUncached :: Path -> MonadStore ValidPathInfo
queryPathInfoUncached p = do
  runOpArgs QueryPathInfo $ do
    putPath p

  valid <- sockGetBool
  unless valid $ error "Path is not valid"

  drv <- sockGetStr
  hash' <- lBSToText <$> sockGetStr
  refs <- sockGetPaths
  regTime <- sockGetInt
  size <- sockGetInt
  ulti <- sockGetBool
  sigs' <- map lBSToText <$> sockGetStrings
  ca' <- lBSToText <$> sockGetStr
  return $ ValidPathInfo {
             path = p
           , deriverVP = mkPath drv
           , narHash = hash'
           , referencesVP = refs
           , registrationTime = regTime
           , narSizeVP = size
           , ultimate = ulti
           , sigs = sigs'
           , ca = ca'
           }

queryReferrers :: Path -> MonadStore PathSet
queryReferrers p = do
  runOpArgs QueryReferrers $ do
    putPath p
  sockGetPaths

queryValidDerivers :: Path -> MonadStore PathSet
queryValidDerivers p = do
  runOpArgs QueryValidDerivers $ do
    putPath p
  sockGetPaths

queryDerivationOutputs :: Path -> MonadStore PathSet
queryDerivationOutputs p = do
  runOpArgs QueryDerivationOutputs $
    putPath p
  sockGetPaths

queryDerivationOutputNames :: Path -> MonadStore PathSet
queryDerivationOutputNames p = do
  runOpArgs QueryDerivationOutputNames $
    putPath p
  sockGetPaths

-- XXX: this is broken as I don't know how to get hashes from paths (fix mkPath)
queryPathFromHashPart :: Digest PathHashAlgo -> MonadStore (Maybe Path)
queryPathFromHashPart d = do
  runOpArgs QueryPathFromHashPart $
    -- TODO: replace `undefined` with digest encoding function when
    --       [issue 24](https://github.com/haskell-nix/hnix-store/issues/24) is
    --       closed
    putByteStringLen $ LBS.fromStrict $ undefined d
  sockGetPath

type Source = () -- abstract binary source
addToStoreNar :: ValidPathInfo -> Source -> RepairFlag -> CheckSigsFlag -> MonadStore ()
addToStoreNar = undefined  -- XXX

type PathFilter = Path -> Bool

addToStore
  :: forall a. (HasDigest a, NamedAlgo a)
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
    putInt 1
    if recursive
      then putInt 1
      else putInt 0

    putByteStringLen (T.encodeUtf8 . T.toLower . T.fromStrict $ algoName @a)

    B.putLazyByteString bs

  fmap (fromMaybe $ error "TODO: Error") sockGetPath


addTextToStore :: LBS.ByteString -> LBS.ByteString -> PathSet -> RepairFlag -> MonadStore (Maybe Path)
addTextToStore name text references' repair = do
  runOpArgs AddTextToStore $ do
    putByteStringLen name
    putByteStringLen text
    putPaths references'
  sockGetPath

buildPaths :: PathSet -> Build.BuildMode -> MonadStore ()
buildPaths ps bm = void $ simpleOpArgs EnsurePath $ do
  putPaths ps
  putInt $ fromEnum bm

buildDerivation :: PathName -> Drv.Derivation -> Build.BuildMode -> MonadStore Build.BuildResult
buildDerivation = undefined  -- XXX

ensurePath :: Path -> MonadStore ()
ensurePath pn = void $ simpleOpArgs EnsurePath $ putPath pn

addTempRoot :: Path -> MonadStore ()
addTempRoot pn = void $ simpleOpArgs AddTempRoot $ putPath pn

addIndirectRoot :: Path -> MonadStore ()
addIndirectRoot pn = void $ simpleOpArgs AddIndirectRoot $ putPath pn

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
  runOpArgs CollectGarbage $ do
    putInt $ fromEnum $ GC.operation opts
    putPaths $ GC.pathsToDelete opts
    putBool $ GC.ignoreLiveness opts
    putInt $ GC.maxFreed opts
    forM_ [(0 :: Int)..2] $ pure $ putInt (0 :: Int) -- removed options

  paths <- sockGetPaths
  freed <- sockGetInt
  _obsolete <- sockGetInt :: MonadStore Int

  return $ GC.Result paths freed

optimiseStore :: MonadStore ()
optimiseStore = void $ simpleOp OptimiseStore

-- returns True on errors
verifyStore :: CheckFlag -> RepairFlag -> MonadStore Bool
verifyStore check repair = simpleOpArgs VerifyStore $ do
  putBool check
  putBool repair

addSignatures :: Path -> [LBS.ByteString] -> MonadStore ()
addSignatures p signatures = void $ simpleOpArgs AddSignatures $ do
  putPath p
  putByteStrings signatures

-- TODO:
queryMissing :: PathSet -> MonadStore (PathSet, PathSet, PathSet, Integer, Integer)
queryMissing ps = undefined --  willBuild willSubstitute unknown downloadSize narSize


