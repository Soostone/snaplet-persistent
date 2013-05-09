{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}


module Snap.Snaplet.Auth.Backends.Persistent.Types where

------------------------------------------------------------------------------
import           Data.Text                    (Text)
import           Data.Time
import           Database.Persist
import           Database.Persist.Quasi
import           Database.Persist.TH          hiding (derivePersistField)
------------------------------------------------------------------------------


share [mkPersist sqlSettings, mkMigrate "migrateAuth"]
      $(persistFileWith lowerCaseSettings "schema.txt")

