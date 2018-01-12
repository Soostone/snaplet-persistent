{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import           Control.Lens
import qualified Data.Text.Encoding as T
import qualified Database.Persist as P
import           Database.Persist.Sql
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.Persistent
import           Snap.Snaplet.Persistent
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
routes = [ ("/",            writeText "hello")
         , ("foo", fooHandler)
         , ("add/:uname", addHandler)
         ]


fooHandler :: Handler App App ()
fooHandler = do
    results <- runPersist $ P.selectList [] []
    liftIO $ print (map db2au results)


addHandler :: Handler App App ()
addHandler = do
    mname <- getParam "uname"
    let name = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser name ""
    liftIO $ print u

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing Nothing
    d <- nestSnaplet "db" db $ initPersist (runMigrationUnsafe migrateAuth)
    a <- nestSnaplet "auth" auth $
           initPersistAuthManager sess (persistPool $ view snapletValue d)
    addRoutes routes
    return $ App s d a


main :: IO ()
main = serveSnaplet defaultConfig app

