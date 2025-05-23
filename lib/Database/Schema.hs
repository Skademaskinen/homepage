{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Schema where

import Database.Persist.TH (mkEntityDefList, mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Time (UTCTime(UTCTime))

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkEntityDefList "defs"] [persistLowerCase|
    Visit                                               sql=visits
        timestamp               Int                     sql=timestamp
        uuid                    String                  sql=uuid
        deriving Eq Show
    GuestbookEntry                                      sql=guestbook
        timestamp               Int                     sql=timestamp
        name                    String                  sql=name
        content                 String                  sql=content
        parentId                Int                     sql=parentId
        deriving Eq Show
    Snake                                               sql=snake
        timestamp               Int                     sql=timestamp
        name                    String                  sql=name
        score                   Int                     sql=score
        speed                   Int                     sql=speed
        fruits                  Int                     sql=fruits
        deriving Eq Show
    User                                                sql=users
        name                    String                  sql=username
        password                String                  sql=password
        Username name
        deriving Eq Show
    Token                                               sql=valid_tokens
        token                   String                  sql=token
        name                    String                  sql=username
        ValidToken token
        deriving Eq Show
    Event                                               sql=events
        date                    UTCTime                 sql=date
        responsible             String                  sql=responsible
        cancelled               Bool                    sql=cancelled
        deriving Eq Show
    Member                                              sql=members
        name                    String                  sql=name
        deriving Eq Show
|]
