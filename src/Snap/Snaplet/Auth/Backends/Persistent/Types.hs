{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Snap.Snaplet.Auth.Backends.Persistent.Types where

------------------------------------------------------------------------------
import           Data.Text              (Text)
import           Data.Time
import           Database.Persist.Quasi
import           Database.Persist.TH    hiding (derivePersistField)
------------------------------------------------------------------------------


share [mkPersist sqlSettings, mkMigrate "migrateAuth"]
      $(persistFileWith lowerCaseSettings "schema.txt")

