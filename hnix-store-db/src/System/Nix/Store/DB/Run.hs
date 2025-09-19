module System.Nix.Store.DB.Run
  ( systemConnectionInfo
  , runSystemSqlite
  , memoryConnectionInfo
  , runInMemory
  , runCustom
  , runWithLogging
  , allMigrations
  , doMigrateAll
  , memTest
  , testMigrateAll
  , test
  , bench
  ) where

import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Default.Class (Default(def))
import Data.Text (Text)
import Database.Persist.Sql (SqlPersistM, SqlBackend, Migration)
import Database.Persist.Sqlite (SqliteConnectionInfo)

import System.Nix.Store.DB.Query

import Control.Monad qualified
import Control.Monad.IO.Class qualified
import Control.Monad.Logger qualified
import Data.ByteString.Char8 qualified
import Database.Esqueleto.Experimental qualified
import Database.Persist.Sql qualified
import Database.Persist.Sqlite qualified
import System.Log.FastLogger qualified
import System.Nix.StorePath qualified
import System.Nix.Store.DB.Schema qualified
import System.Nix.Store.DB.Util qualified

-- | @SqliteConnectionInfo@ for accessing
-- systems database in /nix/var/nix/db/db.sqlite
-- Currently set to immutable
systemConnectionInfo :: SqliteConnectionInfo
systemConnectionInfo =
  Database.Persist.Sqlite.mkSqliteConnectionInfo
    "file:/nix/var/nix/db/db.sqlite?immutable=1"

-- | Run with @systemConnectionInfo@
runSystemSqlite
  :: SqlPersistM a
  -> IO a
runSystemSqlite =
  Database.Persist.Sqlite.runSqliteInfo
    systemConnectionInfo

-- | @SqliteConnectionInfo@ for running in memory
memoryConnectionInfo :: SqliteConnectionInfo
memoryConnectionInfo =
  Database.Persist.Sqlite.mkSqliteConnectionInfo
    ":memory:"

-- | Run with @memoryConnectionInfo@
runInMemory
  :: SqlPersistM a
  -> IO a
runInMemory =
  Database.Persist.Sqlite.runSqliteInfo
    memoryConnectionInfo

-- | Run with custom connection string
runCustom
  :: Text
  -> SqlPersistM a
  -> IO a
runCustom =
  Database.Persist.Sqlite.runSqlite

-- | Run with logging
runWithLogging
  :: MonadUnliftIO m
  => SqliteConnectionInfo
  -> ReaderT SqlBackend (LoggingT m) a
  -> m a
runWithLogging connInfo act = do
  flip Control.Monad.Logger.runLoggingT
    (\_ _ _ s ->
      Data.ByteString.Char8.putStrLn
      $ System.Log.FastLogger.fromLogStr s
    )
    $ Database.Persist.Sqlite.withSqliteConnInfo
        connInfo
        $ Database.Persist.Sql.runSqlConn act

-- | Test that we can create in-memory database
-- and run a dummy query, used by smoke test
memTest :: IO ()
memTest = runInMemory $ do
  doMigrateAll
  _ <- queryEverything
  pure ()

allMigrations :: Migration
allMigrations = do
  let addSafeMigration
          -- the False means it is not unsafe to run (idempotent)
        = Database.Persist.Sql.addMigration False

  System.Nix.Store.DB.Schema.migrateAll

  addSafeMigration
    "CREATE INDEX IF NOT EXISTS IndexReferrer ON Refs(referrer)"
  addSafeMigration
    "CREATE INDEX IF NOT EXISTS IndexReference ON Refs(reference)"
  addSafeMigration
    "CREATE INDEX IF NOT EXISTS IndexDerivationOutputs ON DerivationOutputs(path)"
  addSafeMigration
    "CREATE TRIGGER IF NOT EXISTS DeleteSelfRefs before delete on ValidPaths \
    \begin delete from Refs where referrer = old.id and reference = old.id; end;"

-- | Perform migration
doMigrateAll
  :: MonadIO m
  => ReaderT SqlBackend m ()
doMigrateAll =
  Database.Persist.Sql.runMigration allMigrations

-- | Perform migration on real database
testMigrateAll :: IO ()
testMigrateAll = do
  let connInfo =
        Database.Persist.Sqlite.mkSqliteConnectionInfo
          "/tmp/db.sqlite"
  runWithLogging
    -- We need to disable foreign key checking otherwise
    -- the migration would fail
    (System.Nix.Store.DB.Util.disableFK connInfo)
    -- this actually returns what queries were performed
    -- during migration so we just discard it
    $ Control.Monad.void
    -- use runMigrationSilent as we have logging enabled
    $ Database.Persist.Sql.runMigrationSilent allMigrations

-- | Elaborate test, testing most available query
-- functionality. Same as README.md (db-readme executable)
test :: IO ()
test = do
  runSystemSqlite $ do
    (paths, refs, drvOuts) <- queryEverything

    Control.Monad.IO.Class.liftIO $ do
      putStrLn $ "Stats: "
      let stat name v = putStrLn $ "- " ++ name ++ ": " ++ show (length v)
      stat "ValidPath(s)" paths
      stat "Ref(s)" refs
      stat "DerivationOutput(s)" drvOuts

    maybeValidPath <- queryOneValidDerivationEntity
    case maybeValidPath of
      Nothing -> pure ()
      Just validPathEntity -> do
        let pth =
              System.Nix.Store.DB.Schema.validPathPath
              $ Database.Esqueleto.Experimental.entityVal validPathEntity

        (same, samePath, references, referrers, validDerivers, outputs) <- (,,,,,)
          <$> queryPathInfo pth
          <*> queryPathFromHashPart def (System.Nix.StorePath.storePathHash pth)
          <*> queryReferences validPathEntity
          <*> queryReferrers pth
          <*> queryValidDerivers pth
          <*> queryDerivationOutputs validPathEntity

        Control.Monad.unless (same == Just (Database.Esqueleto.Experimental.entityVal validPathEntity))
          $ error "queryPathInfo failed to roundtrip"
        Control.Monad.unless (samePath == Just pth)
          $ error "queryPathFromHashPart failed to roundtrip"

        Control.Monad.IO.Class.liftIO $ do
          putStrLn $ "References: "
          print references
          putStrLn $ "Referrers: "
          print referrers
          putStrLn $ "Valid derivers: "
          print validDerivers
          putStrLn $ "Derivation outputs: "
          print outputs

    pure ()

-- | Query everything and for each valid path
-- perform detailed queries
bench :: IO ()
bench = do
  runSystemSqlite $ do
    (paths, refs, drvOuts) <- queryEverything
    Control.Monad.IO.Class.liftIO $ do
      putStrLn $ "Stats: "
      let stat name v = putStrLn $ "- " ++ name ++ ": " ++ show (length v)
      stat "ValidPath(s)" paths
      stat "Ref(s)" refs
      stat "DerivationOutput(s)" drvOuts

    Control.Monad.forM_ paths proc
  where
    proc validPathEntity = do
      let pth =
            System.Nix.Store.DB.Schema.validPathPath
            $ Database.Esqueleto.Experimental.entityVal validPathEntity

      (same, samePath, _references, _referrers, _validDerivers, _outputs) <- (,,,,,)
        <$> queryPathInfo pth
        <*> queryPathFromHashPart def (System.Nix.StorePath.storePathHash pth)
        <*> queryReferences validPathEntity
        <*> queryReferrers pth
        <*> queryValidDerivers pth
        <*> queryDerivationOutputs validPathEntity

      Control.Monad.unless (same == Just (Database.Esqueleto.Experimental.entityVal validPathEntity))
        $ error "queryPathInfo failed to roundtrip"
      Control.Monad.unless (samePath == Just pth)
        $ error "queryPathFromHashPart failed to roundtrip"
