{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Persistent where

-------------------------------------------------------------------------------
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Configurator
import Database.Persist.Postgresql hiding (get)
import Snap.Snaplet
import Paths_snaplet_persistent
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


-------------------------------------------------------------------------------
initPersist :: Migration (SqlPersist IO) -> SnapletInit b PersistState
initPersist migration = makeSnaplet "persist" description datadir $ do
    p <- mkSnapletPgPool
    liftIO $ runSqlPool (runMigrationUnsafe migration) p
    return $ PersistState p
  where
    description = "Snaplet for persistent DB library"
    datadir = Just $ liftM (++"/resources/db") getDataDir


-------------------------------------------------------------------------------
mkSnapletPgPool :: (MonadIO (m b v), MonadSnaplet m) => m b v ConnectionPool
mkSnapletPgPool = do
  conf <- getSnapletUserConfig
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  createPostgresqlPool pgConStr cons


-------------------------------------------------------------------------------
runPersist :: SqlPersist (ResourceT IO) a -> Handler b (PersistState) a
runPersist action = do
  pool <- gets persistPool
  liftIO . runResourceT $ runSqlPool action pool

