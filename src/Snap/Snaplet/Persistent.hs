{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Snap.Snaplet.Persistent
  ( initPersist
  , PersistState(..)
  , HasPersistPool(..)
  , mkSnapletPool
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
  , fromPersistValue'
  ) where

-------------------------------------------------------------------------------
import           Control.Monad.Logger
import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Configurator.Types
import           Data.Maybe
import           Data.Readable
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Word
import           Database.Persist
import           Database.Persist.Sql
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
-- | Initialize Persistent with a function to create an Persistent connection
-- pool from Config and an initial SQL function called right  after the
-- connection pool has  been created. This is most useful for calling migrations
-- upfront right after initialization.
--
-- Example:
--
-- > initPersist mkSnapletPgPool (runMigrationUnsafe migrateAll)
--
-- where migrateAll is the migration function that was auto-generated
-- by the QQ statement in your persistent schema definition in the
-- call to 'mkMigrate'.
initPersist :: (Config -> Initializer b PersistState ConnectionPool) -> SqlPersistT (NoLoggingT IO) a -> SnapletInit b PersistState
initPersist mkPool migration = makeSnaplet "persist" description datadir $ do
    p <- mkSnapletPool mkPool
    _ <- liftIO . runNoLoggingT $ runSqlPool migration p
    return $ PersistState p
  where
    description = "Snaplet for persistent DB library"
    datadir = Just $ liftM (++"/resources/db") getDataDir

-------------------------------------------------------------------------------
-- | Conscruts a connection pool in a snaplet context.
mkSnapletPool :: (MonadIO (m b v), MonadSnaplet m) => (Config -> m b v ConnectionPool) -> m b v ConnectionPool
mkSnapletPool mkPool = do
  conf <- getSnapletUserConfig
  mkPool conf

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
-- | Make a Key from an Int.
mkKey :: Int -> Key entity
mkKey = Key . toPersistValue


-------------------------------------------------------------------------------
-- | Makes a Key from a ByteString.  Calls error on failure.
mkKeyBS :: ByteString -> Key entity
mkKeyBS = mkKey . fromMaybe (error "Can't ByteString value") . fromBS


-------------------------------------------------------------------------------
-- | Makes a Key from Text.  Calls error on failure.
mkKeyT :: Text -> Key entity
mkKeyT = mkKey . fromMaybe (error "Can't Text value") . fromText


-------------------------------------------------------------------------------
-- | Makes a Text representation of a Key.
showKey :: Key e -> Text
showKey = T.pack . show . mkInt


-------------------------------------------------------------------------------
-- | Makes a ByteString representation of a Key.
showKeyBS :: Key e -> ByteString
showKeyBS = T.encodeUtf8 . showKey


-------------------------------------------------------------------------------
-- | Converts a Key to Int.  Fails with error if the conversion fails.
mkInt :: Key a -> Int
mkInt = fromPersistValue' . unKey


-------------------------------------------------------------------------------
-- | Converts a Key to Word64.  Fails with error if the conversion fails.
mkWord64 :: Key a -> Word64
mkWord64 = fromPersistValue' . unKey


-------------------------------------------------------------------------------
-- Converts a PersistValue to a more concrete type.  Calls error if the
-- conversion fails.
fromPersistValue' :: PersistField c => PersistValue -> c
fromPersistValue' = either (const $ error "Persist conversion failed") id
                    . fromPersistValue
