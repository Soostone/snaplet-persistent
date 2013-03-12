{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Persistent where

-------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Maybe
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word
import           Database.Persist.GenericSql.Raw
import           Database.Persist.Postgresql hiding (get)
import qualified Database.Persist.Postgresql as DB
import           Database.Persist.Store
import           Snap.Snaplet
import           Paths_snaplet_persistent
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


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
initPersist :: SqlPersist IO a -> SnapletInit b PersistState
initPersist migration = makeSnaplet "persist" description datadir $ do
    p <- mkSnapletPgPool
    liftIO $ runSqlPool migration p
    return $ PersistState p
  where
    description = "Snaplet for persistent DB library"
    datadir = Just $ liftM (++"/resources/db") getDataDir


-------------------------------------------------------------------------------
mkPgPool :: MonadIO m => Config -> m ConnectionPool
mkPgPool conf = do
  pgConStr <- liftIO $ require conf "postgre-con-str"
  cons <- liftIO $ require conf "postgre-pool-size"
  createPostgresqlPool pgConStr cons


-------------------------------------------------------------------------------
mkSnapletPgPool :: (MonadIO (m b v), MonadSnaplet m) => m b v ConnectionPool
mkSnapletPgPool = do
  conf <- getSnapletUserConfig
  mkPgPool conf


-------------------------------------------------------------------------------
runPersist :: (HasPersistPool m)
           => SqlPersist (ResourceT (NoLoggingT IO)) b -> m b
runPersist action = do
  pool <- getPersistPool
  liftIO . runNoLoggingT . runResourceT $ runSqlPool action pool


-------------------------------------------------------------------------------
mkKey :: Int -> Key entity
mkKey = Key . toPersistValue


-------------------------------------------------------------------------------
mkKeyBS :: ByteString -> Key entity
mkKeyBS = mkKey . fromMaybe (error "Can't ByteString value") . fromBS


-------------------------------------------------------------------------------
mkKeyT :: Text -> Key entity
mkKeyT = mkKey . fromMaybe (error "Can't Text value") . fromText


-------------------------------------------------------------------------------
showKey :: Key e -> Text
showKey = T.pack . show . mkInt


-------------------------------------------------------------------------------
showKeyBS :: Key e -> ByteString
showKeyBS = T.encodeUtf8 . showKey


-------------------------------------------------------------------------------
mkInt :: Key a -> Int
mkInt = fromPersistValue' . unKey


-------------------------------------------------------------------------------
mkWord64 :: Key a -> Word64
mkWord64 = fromPersistValue' . unKey


-------------------------------------------------------------------------------
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


