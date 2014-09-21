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
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Maybe
import           Data.Readable
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Word
import           Database.Persist
import           Database.Persist.Postgresql  hiding (get)
import qualified Database.Persist.Postgresql  as DB
import           Paths_snaplet_persistent
import           Snap.Snaplet
import           Snap.Snaplet.Persistent
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Initialize Postgres-Persistent with an initial SQL function called right
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
initPersistPg :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersistPg migration = initPersist mkPgPool migration

-------------------------------------------------------------------------------
-- | Constructs a connection pool from Config.
mkPgPool :: MonadIO m => Config -> m ConnectionPool
mkPgPool conf = do
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  createPostgresqlPool pgConStr cons


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
