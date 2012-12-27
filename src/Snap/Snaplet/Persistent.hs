{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Persistent where

-------------------------------------------------------------------------------
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Configurator
import Database.Persist.Postgresql hiding (get)
import Snap.Snaplet
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


-------------------------------------------------------------------------------
initPersist :: Migration (SqlPersist IO) -> SnapletInit b PersistState
initPersist migration = makeSnaplet "persist" "Snaplet for persistent DB library" Nothing $ do
    p <- mkSnapletPgPool
    liftIO $ runSqlPool (runMigrationUnsafe migration) p
    return $ PersistState p


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

