{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Snaplet.Persistent
  ( initPersist
  , PersistState(..)
  , HasPersistPool(..)
  , mkPgPool
  , mkSnapletPgPool
  , runPersist
  , withPool

  -- * Utility Functions
  , followForeignKey
  , fromPersistValue'
  ) where

-------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Configurator
import           Data.Configurator.Types
import           Database.Persist
import           Database.Persist.Postgresql  hiding (get)
import qualified Database.Persist.Postgresql  as DB
import           Paths_snaplet_persistent
import           Snap.Snaplet
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


-------------------------------------------------------------------------------
-- | Implement this type class to have any monad work with snaplet-persistent.
-- A default instance is provided for (Handler b PersistState).
class MonadIO m => HasPersistPool m where
    getPersistPool :: m ConnectionPool


instance HasPersistPool m => HasPersistPool (NoLoggingT m) where
    getPersistPool = runNoLoggingT getPersistPool

instance HasPersistPool (Handler b PersistState) where
    getPersistPool = gets persistPool

instance MonadIO m => HasPersistPool (ReaderT ConnectionPool m) where
    getPersistPool = ask


-------------------------------------------------------------------------------
-- | Initialize Persistent with an initial SQL function called right
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
initPersist :: SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersist migration = makeSnaplet "persist" description datadir $ do
    conf <- getSnapletUserConfig
    p <- liftIO . runNoLoggingT $ mkSnapletPgPool conf

    liftIO . runNoLoggingT $ runSqlPool migration p
    return $ PersistState p
  where
    description = "Snaplet for persistent DB library"
    datadir = Just $ liftM (++"/resources/db") getDataDir


-------------------------------------------------------------------------------
-- | Constructs a connection pool from Config.
mkPgPool :: (MonadLogger m, MonadBaseControl IO m, MonadIO m) => Config -> m ConnectionPool
mkPgPool conf = do
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  createPostgresqlPool pgConStr cons

-------------------------------------------------------------------------------
-- | Constructs a connection pool in a snaplet context.
mkSnapletPgPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m) => Config -> m ConnectionPool
mkSnapletPgPool = mkPgPool

-------------------------------------------------------------------------------
-- | Runs a SqlPersist action in any monad with a HasPersistPool instance.
runPersist :: (HasPersistPool m)
           => SqlPersistT (ResourceT (NoLoggingT IO)) b
           -- ^ Run given Persistent action in the defined monad.
           -> m b
runPersist action = do
  pool <- getPersistPool
  withPool pool action


------------------------------------------------------------------------------
-- | Run a database action
withPool :: MonadIO m
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> m a
withPool cp f = liftIO . runNoLoggingT . runResourceT $ runSqlPool f cp

-------------------------------------------------------------------------------
-- Converts a PersistValue to a more concrete type.  Calls error if the
-- conversion fails.
fromPersistValue' :: PersistField c => PersistValue -> c
fromPersistValue' = either (const $ error "Persist conversion failed") id
                    . fromPersistValue


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


