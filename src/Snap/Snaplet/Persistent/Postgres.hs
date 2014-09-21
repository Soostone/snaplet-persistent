{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Persistent.Postgres
  ( initPersistPg
  , PersistState(..)
  , HasPersistPool(..)
  , mkPgPool
  , mkSnapletPgPool
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
import           Database.Persist (PersistEntity, PersistEntityBackend, Entity(..), Key)
import           Database.Persist.Postgresql (SqlPersistT, ConnectionPool, SqlBackend)
import qualified Database.Persist.Postgresql  as DB
import           Snap.Snaplet (SnapletInit, MonadSnaplet, getSnapletUserConfig)
import           Snap.Snaplet.Persistent
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Initialize Postgres-Persistent with an initial SQL function called right
-- after the connection pool has been created. This is most useful for
-- calling migrations upfront right after initialization.
--
-- Example:
--
-- > initPersistPg (runMigrationUnsafe migrateAll)
--
-- where migrateAll is the migration function that was auto-generated
-- by the QQ statement in your persistent schema definition in the
-- call to 'mkMigrate'.
initPersistPg :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistPg migration = initPersist mkPgPool migration

-------------------------------------------------------------------------------
-- | Constructs a connection pool from Config.
mkPgPool :: MonadIO m => Config -> m ConnectionPool
mkPgPool conf = do
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  DB.createPostgresqlPool pgConStr cons


-------------------------------------------------------------------------------
-- | Conscruts a Postgres connection pool in a snaplet context.
mkSnapletPgPool :: (MonadIO (m b v), MonadSnaplet m) => m b v ConnectionPool
mkSnapletPgPool = do
  conf <- getSnapletUserConfig
  mkPgPool conf

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
