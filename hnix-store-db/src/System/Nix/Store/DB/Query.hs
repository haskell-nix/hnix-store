{-# LANGUAGE Rank2Types #-}

module System.Nix.Store.DB.Query
  ( queryPathInfoEntity
  , queryPathInfo
  , queryReferencesEntity
  , queryReferences
  , queryReferrersEntity
  , queryReferrers
  , queryValidDerivers
  , queryDerivationOutputs
  , queryPathFromHashPart
  , queryValidPathsEntity
  , queryValidPaths
  -- * Testing
  , queryAllRefsEntity
  , queryAllRefs
  , queryAllDerivationOutputsEntity
  , queryAllDerivationOutputs
  , queryOneValidDerivationEntity
  , queryOneValidDerivation
  , queryEverything
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Data.Text (Text)
import Database.Esqueleto.Experimental
import System.Nix.StorePath (StoreDir, StorePath, StorePathHashPart)
import System.Nix.Store.DB.Schema

import qualified Data.ByteString.Char8
import qualified Data.Maybe
import qualified Data.Text
import qualified System.Nix.StorePath
import qualified System.Nix.StorePath.Metadata

-- * Queries

-- | Query @Entity ValidPath@ for @StorePath@ if it exists.
queryPathInfoEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => StorePath
  -> SqlReadT m (Maybe (Entity ValidPath))
queryPathInfoEntity path = do
  res <- select $ do
    validPaths <- from $ table @ValidPath
    where_ (validPaths ^. ValidPathPath ==. val path)
    pure validPaths
  pure $ Data.Maybe.listToMaybe res

-- | Query @ValidPath@ for @StorePath@ if it exists.
queryPathInfo
  :: ( MonadIO m
     , MonadLogger m
     )
  => StorePath
  -> SqlReadT m (Maybe ValidPath)
queryPathInfo sp =
  -- this is expanded from >>= pure $ (fmap . fmap) entityVal
  -- to make older GHCs (like 9.0.2) happy
  queryPathInfoEntity sp >>= \case
    Nothing -> pure Nothing
    Just evp -> pure $ Just (entityVal evp)

-- | Query references as a list of @Entity Ref@s for @ValidPath@
-- using id of @Entity ValidPath@
queryReferencesEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => Entity ValidPath
  -> SqlReadT m [Entity Ref]
queryReferencesEntity referrer =
  select $ do
    (refs :& _validPaths) <-
      from $ table @Ref
      `innerJoin` table @ValidPath
      `on` (\(refs :& validPaths) ->
        refs ^. RefReference ==. validPaths ^. ValidPathId)
    where_ (refs ^. RefReferrer ==. val (entityKey referrer))
    pure refs

-- | Query references as a list of @Ref@s for @ValidPath@
-- by id of @Entity ValidPath@
queryReferences
  :: ( MonadIO m
     , MonadLogger m
     )
  => Entity ValidPath
  -> SqlReadT m [Ref]
queryReferences evp = do
  queryReferencesEntity evp >>= pure . fmap entityVal

-- | Query referrers as a list of @Entity Ref@s for @StorePath@
queryReferrersEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => StorePath
  -> SqlReadT m [Entity Ref]
queryReferrersEntity path = do
  select $ do
    (refs :& _validPaths) <-
      from $ table @Ref
      `innerJoin` table @ValidPath
      `on` (\(refs :& validPaths) ->
        (refs ^. RefReference ==. validPaths ^. ValidPathId))
    where_
      (
        refs ^. RefReference
        `in_`
        (subList_select $ do
          validPaths <- from $ table @ValidPath
          where_ (validPaths ^. ValidPathPath ==. val path)
          pure $ validPaths ^. ValidPathId
        )
      )
    pure refs

-- | Query referrers as a list of @Ref@s for @StorePath@
queryReferrers
  :: ( MonadIO m
     , MonadLogger m
     )
  => StorePath
  -> SqlReadT m [Ref]
queryReferrers sp =
  queryReferrersEntity sp
  >>= pure . (fmap entityVal)

-- | Query valid derivers as a list of @(Text, StorePath)@s
-- for some @StorePath@
queryValidDerivers
  :: ( MonadIO m
     , MonadLogger m
     )
  => StorePath
  -> SqlReadT m [(Text, StorePath)]
queryValidDerivers path = do
  res <- select $ do
    (drvOuts :& _validPaths) <-
      from $ table @DerivationOutput
      `innerJoin` table @ValidPath
      `on` (\(drvOuts :& validPaths) ->
        (drvOuts ^. DerivationOutputDrv ==. validPaths ^. ValidPathId))
    where_ (drvOuts ^. DerivationOutputPath ==. val path)
    pure (drvOuts ^. DerivationOutputName, drvOuts ^. DerivationOutputPath)

  pure $ unValue2 <$> res

-- | Query derivation outputs as a list of @(Text, StorePath)@s
-- for some @ValidPath@ by its id
queryDerivationOutputs
  :: ( MonadIO m
     , MonadLogger m
     )
  => Entity ValidPath
  -> SqlReadT m [(Text, StorePath)]
queryDerivationOutputs drv = do
  res <- select $ do
    drvOuts <- from $ table @DerivationOutput
    where_ (drvOuts ^. DerivationOutputDrv ==. val (entityKey drv))
    pure (drvOuts ^. DerivationOutputName, drvOuts ^. DerivationOutputPath)

  pure $ unValue2 <$> res

-- | Query @StorePath@ from its hash part
queryPathFromHashPart
  :: ( MonadIO m
     , MonadLogger m
     )
  => StoreDir
  -> StorePathHashPart
  -> SqlReadT m (Maybe StorePath)
queryPathFromHashPart storeDir hp =
  let hashPart =
        ( Data.Text.pack
        . Data.ByteString.Char8.unpack
        $ System.Nix.StorePath.unStoreDir storeDir
        )
        <> "/"
        <> System.Nix.StorePath.storePathHashPartToText hp
  in do
    -- We use rawSql here
    -- as otherwise, we would have to construct a @StorePath@
    -- to match the type of ValidPath.path, but the @StorePath@
    -- always includes name, so we would have to change
    -- the type of ValidPath.path to @Either StorePathHashPart StorePath@
    -- which isn't worth for a single query
    raw <- rawSql
            "select ?? from ValidPaths where path >= ? limit 1"
            [PersistText hashPart]
    pure
      $ Data.Maybe.listToMaybe
      $ validPathPath . entityVal
      <$> raw

-- | Query all valid paths as a list of @Entity ValidPath@s
queryValidPathsEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [Entity ValidPath]
queryValidPathsEntity =
  select $ from $ table @ValidPath

-- | Query all valid paths as a list of @ValidPath@s
queryValidPaths
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [ValidPath]
queryValidPaths =
  queryValidPathsEntity
  >>= pure . fmap entityVal

-- * For testing

-- | Query all references as a list of @Entity Ref@s
queryAllRefsEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [Entity Ref]
queryAllRefsEntity =
  select $ from $ table @Ref

-- | Query all references as a list of @Ref@s
queryAllRefs
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [Ref]
queryAllRefs =
  queryAllRefsEntity
  >>= pure . fmap entityVal

-- | Query all derivation outputs as a list of @Entity DerivationOutput@s
queryAllDerivationOutputsEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [Entity DerivationOutput]
queryAllDerivationOutputsEntity =
  select $ from $ table @DerivationOutput

-- | Query all derivation outputs as a list of @DerivationOutput@s
queryAllDerivationOutputs
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m [DerivationOutput]
queryAllDerivationOutputs =
  queryAllDerivationOutputsEntity
  >>= pure . fmap entityVal

-- | Query one random derivation as an @Entity ValidPath@
queryOneValidDerivationEntity
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m (Maybe (Entity ValidPath))
queryOneValidDerivationEntity = do
  res <- select $ do
    validPath <- from $ table @ValidPath -- \validPath -> do
    where_
      (
        validPath ^. ValidPathUltimate
        ==. (val $ Just System.Nix.StorePath.Metadata.BuiltLocally)
      )
    offset 100
    limit 1
    pure validPath

  pure $ Data.Maybe.listToMaybe res

-- | Query one random derivation as a @ValidPath@
queryOneValidDerivation
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m (Maybe ValidPath)
queryOneValidDerivation =
  queryOneValidDerivationEntity
  >>= pure . fmap entityVal

-- | Query everything
queryEverything
  :: ( MonadIO m
     , MonadLogger m
     )
  => SqlReadT m
       ( [Entity ValidPath]
       , [Entity Ref]
       , [Entity DerivationOutput]
       )
queryEverything = (,,)
  <$> queryValidPathsEntity
  <*> queryAllRefsEntity
  <*> queryAllDerivationOutputsEntity

-- * Utility

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (a, b) = (unValue a, unValue b)
