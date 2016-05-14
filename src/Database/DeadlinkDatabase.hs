{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : DeadlinkDatabase
Description : Deadlink database management
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Functions dealing with Deadlink database management
-}
module Database.DeadlinkDatabase where

import Data.Text (Text)
import Data.FileEmbed(embedStringFile)
import Database.SQLite3 (open, exec, close)

createDeadlinkDB :: Text -> IO ()
createDeadlinkDB dbname = do
    db <- open dbname
    exec db $(embedStringFile "src/Database/createdb.sql")
    close db
