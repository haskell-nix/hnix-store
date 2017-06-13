{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{- |
Module      : Nix.LocalStore
Description : Local implementation of the nix store API
Copyright   : Copyright (c) 2014 Shea Levy
License     : MIT
Maintainer  : shea@shealevy.com
Stability   : Experimental
Portability : Portable

An implementation of the nix store API that interacts directly with the nix
store and nix database.
-}
module Nix.LocalStore
    ( -- * The @LocalStore@ type
      LocalStore
    , allocateLocalStore
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashSet as HS
import Control.Monad.Trans.Resource
    ( MonadResource
    , ReleaseKey
    , allocate
    )
import qualified Database.SQLite.Simple as DB
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Nix.Types (PathSet, parseHash, parseHashType)
import qualified Nix.Store as S

-- | Instance of the 'Store' class that directly runs builders and modifies
-- the database
data LocalStore = LocalStore
    { connection :: !DB.Connection  -- ^ The database connection
    }

-- | Allocate a @LocalStore@ within a @MonadResource@.
allocateLocalStore :: MonadResource m
                   => FilePath  -- ^ Path to the database
                   -> m (ReleaseKey, LocalStore)
allocateLocalStore path = allocate alloc dealloc
  where
    alloc = DB.open path >>= return . LocalStore
    dealloc (LocalStore c) = DB.close c

-- | Helper for the common case of queries returning @PathSet@
extractPathSet :: [DB.Only FilePath] -> IO PathSet
extractPathSet = return . HS.fromList . map DB.fromOnly

instance S.Store LocalStore where
    isValidPath (LocalStore c) path =
        DB.query c "SELECT EXISTS (SELECT 1 FROM validpaths WHERE path=? LIMIT 1)" (DB.Only path)
            >>= return . DB.fromOnly . head

    queryValidPaths (LocalStore c) paths
        | HS.null paths = return paths
        | otherwise     =
            DB.query c (DB.Query qfinal) (HS.toList paths) >>= extractPathSet
      where
        qbase = "SELECT path FROM validpaths WHERE path IN ("
        qvariable = T.intersperse ',' . flip T.replicate "?" $ HS.size paths
        qfinal = T.append qbase $ T.snoc qvariable ')'

    queryAllValidPaths (LocalStore c) =
        DB.query_ c "SELECT path FROM validpaths" >>= extractPathSet

    queryPathInfo (LocalStore c) path = do
        r <- DB.query c
            "SELECT vp.hash, vp.registrationTime, vp.deriver, vp.narSize, ref.path \
            \FROM validpaths vp \
            \LEFT JOIN refs ON vp.id = refs.referrer \
            \LEFT JOIN validpaths ref ON ref.id = refs.reference \
            \WHERE vp.path=?" (DB.Only path)
        case r of
            (hashString, registrationTime, deriver, narSize, ref) : xs -> return . Just $ S.ValidPathInfo
                { S.path = path
                , S.deriver = deriver
                , S.hash = parseHashField $ encodeUtf8 hashString
                , S.references = case ref of
                      Nothing -> HS.empty
                      Just x -> HS.insert x . HS.fromList $ map extractRef xs
                , S.registrationTime = posixSecondsToUTCTime $ realToFrac (registrationTime :: Int)
                , S.narSize = narSize
                }
            [] -> return Nothing
      where
        extractRef (_, _, _, _, Just ref) = ref
        extractRef _ = error "Impossible null returned from outer join"
        colon = 0x3A  -- ':'
        parseHashField s = case BS.elemIndex colon s of
            Nothing -> error $ "Invalid hash field in database: " ++ (show s)
            Just n -> parseHash (parseHashType $ BS.take n s) $ BS.drop (n + 1) s

    queryReferrers (LocalStore c) path =
        DB.query c
            "SELECT ref.path \
            \FROM validpaths vp \
            \JOIN refs ON vp.id = refs.referrer \
            \JOIN validpaths ref ON ref.id = refs.reference \
            \WHERE vp.path=?" (DB.Only path)
            >>= extractPathSet
