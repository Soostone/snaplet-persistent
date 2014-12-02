{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Persistent.Sqlite
  ( initPersistSqlite
  , initPersistSqliteWith
  , PersistState(..)
  , HasPersistPool(..)
  , mkSqlitePool
  , mkSqlitePoolWith
  , mkSnapletSqlitePool
  , runPersist
  , withPool
  -- * Utility Functions
  , mkKey
  , mkKeyBS
  , mkKeyT
  , showKey
  , showKeyBS
  , mkInt
  , mkWord64
  , followForeignKey
  , fromPersistValue'
  ) where

-------------------------------------------------------------------------------
import           Control.Monad.Logger (NoLoggingT)
import           Control.Monad.State (MonadIO, liftIO)
import           Data.Configurator (require)
import           Data.Configurator.Types (Config)
import           Data.Text (Text)
import           Database.Persist (PersistEntity, PersistEntityBackend, Key, Entity(..))
import           Database.Persist.Sqlite (ConnectionPool, SqlBackend, SqlPersistT)
import qualified Database.Persist.Sqlite  as DB
import           Snap.Snaplet (SnapletInit, MonadSnaplet, getSnapletUserConfig)
import           Snap.Snaplet.Persistent
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Initialize Sqlite-Persistent with an initial SQL function called right
-- after the connection pool has been created. This is most useful for
-- calling migrations upfront right after initialization.
--
-- Example:
--
-- > initPersist (runMigrationUnsafe migrateAll)
--
-- where migrateAll is the migration function that was auto-generated
-- by the QQ statement in your persistent schema definition in the
-- call to 'mkMigrate'.
initPersistSqliteWith :: Text -> Int -> SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistSqliteWith conStr cons migration =  initPersistWith mkSqlitePoolWith conStr cons migration

-- | Initialize Sqlite-Persistent with an initial SQL function called right
-- after the connection pool has been created. This is most useful for
-- calling migrations upfront right after initialization.
--
-- Example:
--
-- > initPersist (runMigrationUnsafe migrateAll)
--
-- where migrateAll is the migration function that was auto-generated
-- by the QQ statement in your persistent schema definition in the
-- call to 'mkMigrate'.
initPersistSqlite :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistSqlite migration =  initPersist mkSqlitePool migration

-------------------------------------------------------------------------------
-- | Constructs a connection pool from Config.
mkSqlitePool :: MonadIO m => Config -> m ConnectionPool
mkSqlitePool conf = do
  sqlConStr <- liftIO $ require conf "sqlite-con-str"
  cons <- liftIO $ require conf "sqlite-pool-size"
  mkSqlitePoolWith sqlConStr cons

-- | Constructs a connection pool from Config.
mkSqlitePoolWith :: MonadIO m => Text -> Int -> m ConnectionPool
mkSqlitePoolWith sqlConStr cons = do
  DB.createSqlitePool sqlConStr cons

-------------------------------------------------------------------------------
-- | Conscruts a Postgres connection pool in a snaplet context.
mkSnapletSqlitePool :: (MonadIO (m b v), MonadSnaplet m) => m b v ConnectionPool
mkSnapletSqlitePool = do
  conf <- getSnapletUserConfig
  mkSqlitePool conf

------------------------------------------------------------------------------
-- | Follows a foreign key field in one entity and retrieves the corresponding
-- entity from the database.
followForeignKey :: (PersistEntity a, HasPersistPool m,
                     PersistEntityBackend a ~ SqlBackend)
                 => (t -> Key a) -> Entity t -> m (Maybe (Entity a))
followForeignKey toKey (Entity _ val) = do
    let key' = toKey val
    mval <- runPersist $ DB.get key'
    return $ fmap (Entity key') mval
