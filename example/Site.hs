{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
<<<<<<< HEAD
import           Control.Applicative((<$>))
import           Control.Monad.Trans
import           Data.ByteString (ByteString, append)
=======
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
>>>>>>> master
import           Control.Lens
import qualified Data.Text.Encoding as T
import qualified Database.Persist as P
import           Database.Persist.Sql
import           Snap
import           Snap.Snaplet.Auth (AuthManager, currentUser, createUser, userLogin, forceLogin)
import           Snap.Snaplet.Auth.Backends.Persistent
<<<<<<< HEAD
import           Snap.Snaplet.Persistent.Sqlite
=======
import           Snap.Snaplet.Persistent.Postgres
>>>>>>> master
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession


------------------------------------------------------------------------------
data App = App
    { _sess :: Snaplet SessionManager
    , _db   :: Snaplet PersistState
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasPersistPool (Handler b App) where
    getPersistPool = with db getPersistPool

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", rootHandler)
         , ("foo", fooHandler)
         , ("add/:uname", addHandler)
         ]

<<<<<<< HEAD
rootHandler :: Handler App App ()
rootHandler = do
  user <- with auth $ currentUser
  maybe
    (writeBS "no user logged in")
    (writeBS . ("current user: " `append`) . T.encodeUtf8 . userLogin) user
=======
>>>>>>> master
fooHandler :: Handler App App ()
fooHandler = do
    results <- runPersist $ P.selectList [] []
    liftIO $ print (map db2au results)
    redirect' "/" 301

addHandler :: Handler App App ()
addHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    x <- with auth $ forceLogin (right u)
    liftIO $ print u
    liftIO $ print x
    redirect' "/" 301
    where right (Right a) = a

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    d <- nestSnaplet "db" db $ initPersistSqliteWith ":memory:" 1 (runMigrationUnsafe migrateAuth)
    a <- nestSnaplet "auth" auth $
           initPersistAuthManager sess (persistPool $ view snapletValue d)
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app

