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

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkEntityDefList "defs"] [persistLowerCase|
    Visit                                   sql=visits
        rid                      Int         sql=id
        timestamp               Int         sql=timestamp
        uuid                    String      sql=uuid
        deriving Eq Show
    GuestbookEntry                          sql=guestbook
        rid                      Int         sql=id
        timestamp               Int         sql=timestamp
        name                    String      sql=name
        content                 String      sql=content
        parentId                Int         sql=parentId
        deriving Eq Show
    Snake                                   sql=snake
        rid                      Int         sql=id
        timestamp               Int         sql=timestamp
        name                    String      sql=name
        score                   Int         sql=score
        speed                   Int         sql=speed
        fruits                  Int         sql=fruits
        deriving Eq Show
    User                                    sql=users
        rid                      Int         sql=id
        name                    String      sql=username
        password                String      sql=password
        deriving Eq Show
    Token                                   sql=valid_tokens
        rid                      Int         sql=id
        token                   String      sql=token
        name                    String      sql=username
        deriving Eq Show
|]
