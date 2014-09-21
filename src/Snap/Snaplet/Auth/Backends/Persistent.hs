{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Snap.Snaplet.Auth.Backends.Persistent
--    ( module Snap.Snaplet.Auth.Backends.Persistent
    ( PersistAuthManager
    , initPersistAuthManager
    , initPersistAuthManager'
    , authEntityDefs

    -- * Persistent Auth Data Types
    -- $datatypes
    , module Snap.Snaplet.Auth.Backends.Persistent.Types
--    , SnapAuthUserGeneric(..)
--    , SnapAuthUser
--    , SnapAuthUserId
    , db2au
    , dbUserSplices
    , userDBKey
    , textPassword
    ) where

------------------------------------------------------------------------------
import           Control.Monad                (liftM)
import           Control.Monad.Trans          (liftIO)
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe                   (fromMaybe, catMaybes)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time                    (UTCTime, getCurrentTime)
import           Database.Persist             ()
import           Database.Persist.Sql
import           Database.Persist.Quasi       (lowerCaseSettings)
import           Database.Persist.TH          hiding (derivePersistField)
import           Heist                        
import           Heist.Compiled               (Splice, deferMap)
import           Paths_snaplet_persistent     (getDataDir)
import           Safe                         (readNote)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Persistent
import           Snap.Snaplet.Session
import           Web.ClientSession            (getKey)
------------------------------------------------------------------------------
import           Snap.Snaplet.Auth.Backends.Persistent.Types 


------------------------------------------------------------------------------
-- | The list of entity definitions this snaplet exposes. You need
-- them so that you can append to your application's list of
-- entity definitions and perform the migration in one block.
--
-- See how this example combined an app's own entity definitions and
-- the auth snaplet's in one migration block:
--
-- > share [mkMigrate "migrateAll"] $
-- >    authEntityDefs ++
-- >    $(persistFileWith lowerCaseSettings "schema.txt")
authEntityDefs :: [EntityDef SqlType]
authEntityDefs = $(persistFileWith lowerCaseSettings "schema.txt")


-- $datatypes
--
-- Persistent creates its own data types mirroring the database schema, so we
-- have to export this extra layer of types and conversion to 'AuthUser'.


------------------------------------------------------------------------------
-- | Function to convert a 'SnapAuthUser' entity into the auth snaplet's
-- 'AuthUser'.
db2au :: Entity SnapAuthUser -> AuthUser
db2au (Entity (Key k) SnapAuthUser{..}) = AuthUser
  { userId               = Just . UserId . fromPersistValue' $ k
  , userLogin            = snapAuthUserLogin
  , userEmail            = Just snapAuthUserEmail
  , userPassword         = Just . Encrypted . T.encodeUtf8
                           $ snapAuthUserPassword
  , userActivatedAt      = snapAuthUserActivatedAt
  , userSuspendedAt      = snapAuthUserSuspendedAt
  , userRememberToken    = snapAuthUserRememberToken
  , userLoginCount       = snapAuthUserLoginCount
  , userFailedLoginCount = snapAuthUserFailedLoginCount
  , userLockedOutUntil   = snapAuthUserLockedOutUntil
  , userCurrentLoginAt   = snapAuthUserCurrentLoginAt
  , userLastLoginAt      = snapAuthUserLastLoginAt
  , userCurrentLoginIp   = T.encodeUtf8 `fmap` snapAuthUserCurrentIp
  , userLastLoginIp      = T.encodeUtf8 `fmap` snapAuthUserLastIp
  , userCreatedAt        = Just snapAuthUserCreatedAt
  , userUpdatedAt        = Just snapAuthUserUpdatedAt
  , userResetToken       = snapAuthUserResetToken
  , userResetRequestedAt = snapAuthUserResetRequestedAt
  , userRoles            = []
  , userMeta             = HM.empty
  }


------------------------------------------------------------------------------
-- | Splices for 'SnapAuthUser' that are equivalent to the ones for
-- 'AuthUser'.
dbUserSplices :: Monad n
              => Splices (RuntimeSplice n (Entity SnapAuthUser) -> Splice n)
dbUserSplices = mapS (deferMap (return . db2au)) userCSplices


data PersistAuthManager = PAM {
      pamPool :: ConnectionPool
      }


------------------------------------------------------------------------------
-- | Initializer that gets AuthSettings from a config file.
initPersistAuthManager :: SnapletLens b SessionManager
                       -> ConnectionPool
                       -> SnapletInit b (AuthManager b)
initPersistAuthManager l pool = make $ do
    aus <- authSettingsFromConfig
    initHelper aus l pool



------------------------------------------------------------------------------
-- | Initializer that lets you specify AuthSettings.
initPersistAuthManager' :: AuthSettings
                        -> SnapletLens b SessionManager
                        -> ConnectionPool
                        -> SnapletInit b (AuthManager b)
initPersistAuthManager' aus l pool = make $ initHelper aus l pool


make :: Initializer b v v -> SnapletInit b v
make = makeSnaplet "persist-auth" description datadir
  where
    description =
      "A snaplet providing user authentication support using Persist"
    datadir = Just $ liftM (++"/resources/auth") getDataDir


initHelper :: AuthSettings
           -> SnapletLens b SessionManager
           -> ConnectionPool
           -> Initializer b (AuthManager b) (AuthManager b)
initHelper aus l pool = liftIO $ do
    key  <- getKey (asSiteKey aus)
    rng <- liftIO mkRNG
    return $ AuthManager {
                   backend = PAM pool
                 , session = l
                 , activeUser = Nothing
                 , minPasswdLen = asMinPasswdLen aus
                 , rememberCookieName = asRememberCookieName aus
                 , rememberPeriod = asRememberPeriod aus
                 , siteKey = key
                 , lockout = asLockout aus
                 , randomNumberGenerator = rng }



readT :: Text -> Int
readT = readNote "Can't read text" . T.unpack


------------------------------------------------------------------------------
-- | Get the db key from an 'AuthUser'
userDBKey :: AuthUser -> Maybe SnapAuthUserId
userDBKey au = case userId au of
                 Nothing -> Nothing
                 Just (UserId k) -> Just . mkKey $ (readT k :: Int)


------------------------------------------------------------------------------
textPassword :: Password -> Text
textPassword (Encrypted bs) = T.decodeUtf8 bs
textPassword (ClearText bs) = T.decodeUtf8 bs


------------------------------------------------------------------------------
-- |
instance IAuthBackend PersistAuthManager where
  save PAM{..} au@AuthUser{..} = do
    now <- liftIO getCurrentTime
    pw <- encryptPassword $ fromMaybe (ClearText "") userPassword
    withPool pamPool $ do
      case userId of
        Nothing -> do
          _ <- insert $ SnapAuthUser
            userLogin
            (fromMaybe "" userEmail)
            (textPassword pw)
            userActivatedAt
            userSuspendedAt
            userRememberToken
            userLoginCount
            userFailedLoginCount
            userLockedOutUntil
            userCurrentLoginAt
            userLastLoginAt
            (fmap T.decodeUtf8 userCurrentLoginIp)
            (fmap T.decodeUtf8 userLastLoginIp)
            now
            now
            Nothing
            Nothing
            ""
            ""
          return $ Right $ au {userUpdatedAt = Just now}
        Just (UserId t) -> do
          let k = (mkKey (readT t :: Int))
          update k $ catMaybes
            [ Just $ SnapAuthUserLogin =. userLogin
            , Just $ SnapAuthUserEmail =. fromMaybe "" userEmail
            , fmap (\ (Encrypted p) -> SnapAuthUserPassword =. T.decodeUtf8 p)
                   userPassword
            , Just $ SnapAuthUserActivatedAt =. userActivatedAt
            , Just $ SnapAuthUserSuspendedAt =. userSuspendedAt
            , Just $ SnapAuthUserRememberToken =. userRememberToken
            , Just $ SnapAuthUserLoginCount =. userLoginCount
            , Just $ SnapAuthUserFailedLoginCount =. userFailedLoginCount
            , Just $ (SnapAuthUserLockedOutUntil =.) userLockedOutUntil
            , Just $ (SnapAuthUserCurrentLoginAt =.) userCurrentLoginAt
            , Just $ (SnapAuthUserLastLoginAt =.) userLastLoginAt
            , Just $ SnapAuthUserCurrentIp =. (T.decodeUtf8 `fmap` userCurrentLoginIp)
            , Just $ (SnapAuthUserLastIp =.) (T.decodeUtf8 `fmap` userLastLoginIp)
            , fmap (SnapAuthUserCreatedAt =.) userCreatedAt
            , Just $ SnapAuthUserUpdatedAt =. now
            , Just $ SnapAuthUserResetToken =. userResetToken
            , Just $ SnapAuthUserResetRequestedAt =. userResetRequestedAt
            , Just $ SnapAuthUserRoles =. show userRoles
            ]
          return $ Right $ au {userUpdatedAt = Just now}


  destroy = fail "We don't allow destroying users."

  lookupByUserId PAM{..} (UserId t) = withPool pamPool $ do
    let k = (mkKey (readT t :: Int))
    u <- get k
    case u of
     Nothing -> return Nothing
     Just u' -> return . Just $ db2au $ Entity k u'

  lookupByLogin PAM{..} login = withPool pamPool $ do
    res <- selectFirst [SnapAuthUserLogin ==. login] []
    return $ fmap db2au res

  lookupByRememberToken PAM{..} token = withPool pamPool $ do
      res <- selectFirst [SnapAuthUserRememberToken ==. Just token] []
      return $ fmap db2au res

