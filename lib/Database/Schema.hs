{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Database.Schema where
import Database.Persist.TH (persistLowerCase, share, mkPersist, sqlSettings, mkMigrate, mkEntityDefList)
    
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkEntityDefList "defs"] [persistLowerCase|
    Visit                                   sql=visits
        visitId                 Int         sql=id
        visitTimestamp          Int         sql=timestamp
        visitUuid               String      sql=uuid
        deriving Eq Show
    GuestbookEntry                          sql=guestbook
        guestbookId             Int         sql=id
        guestbookTimestamp      Int         sql=timestamp
        guestbookName           String      sql=name
        guestbookContent        String      sql=content
        guestbookParentId       Int         sql=parentId
        deriving Eq Show
    Snake                                   sql=snake
        snakeId                 Int         sql=id
        snakeTimestamp          Int         sql=timestamp
        snakeName               String      sql=name
        snakeScore              Int         sql=score
        snakeSpeed              Int         sql=speed
        snakeFruits             Int         sql=fruits
        deriving Eq Show
    User                                    sql=users
        userId                  Int         sql=id
        userName                String      sql=username
        userPassword            String      sql=password
        deriving Eq Show
    Token                                   sql=valid_tokens
        tokenId                 Int         sql=id
        tokenToken              String      sql=token
        tokenName               String      sql=username
|]
