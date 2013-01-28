{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Persistent where

-------------------------------------------------------------------------------
import           Control.Monad.State
import           Control.Monad.Trans.Resource
import           Data.ByteString (ByteString)
import           Data.Configurator
import           Data.Maybe
import           Data.Readable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Word
import           Database.Persist.Postgresql hiding (get)
import           Database.Persist.Store
import           Snap.Snaplet
import           Paths_snaplet_persistent
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype PersistState = PersistState { persistPool :: ConnectionPool }


-------------------------------------------------------------------------------
-- | Initialize Persistent with an initial SQL function called right
-- after the connection pool has been created. This is most useful for
-- calling migrations upfront right after initialization.
--
-- Example:
-- > initPersist (runMigration migrateAll)
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

