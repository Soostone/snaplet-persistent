{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Snap.Snaplet.Persistent
  ( initPersist
  , PersistState(..)
  , HasPersistPool(..)
  , mkPgPool
  , mkSnapletPgPool
  , runPersist
  , withPool
  ) where

-------------------------------------------------------------------------------
import qualified Control.Monad.Catch          as EC (Handler (..),
                                                     MonadCatch (..))
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Configurator
import           Data.Configurator.Types
import           Database.Persist.Postgresql  hiding (get)
import           Database.Persist.Sql         ()
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
mkSnapletPgPool :: (MonadBaseControl IO m, MonadLogger m, MonadIO m, EC.MonadCatch m) => Config -> m ConnectionPool
mkSnapletPgPool = mkPgPool

-------------------------------------------------------------------------------
-- | Runs a SqlPersist action in any monad with a HasPersistPool instance.
runPersist :: (HasPersistPool m, EC.MonadCatch m)
           => SqlPersistT (ResourceT (NoLoggingT IO)) b
           -- ^ Run given Persistent action in the defined monad.
           -> m b
runPersist action = do
  pool <- getPersistPool
  withPool pool action


------------------------------------------------------------------------------
-- | Run a database action, if a `PersistentSqlException` is raised
-- the action will be retried four times with a 50ms delay between
-- each retry.
--
-- This is being done because sometimes Postgres will reap connections
-- and the connection leased out of the pool may then be stale and
-- will often times throw a `Couldn'tGetSQLConnection` type value.
withPool :: (MonadIO m, EC.MonadCatch m)
         => ConnectionPool
         -> SqlPersistT (ResourceT (NoLoggingT IO)) a -> m a
withPool cp f = do
    -- TODO: `withPool` is a bad name for this, shouldn't it be
    -- `withPG`?
    recovering retryPolicy [isPGExc] runF
  where
    retryPolicy = constantDelay 50000 <> limitRetries 4
    isPGExc _   = EC.Handler $ \(_ :: PersistentSqlException) -> return True
    runF        = liftIO . runNoLoggingT . runResourceT $ runSqlPool f cp
