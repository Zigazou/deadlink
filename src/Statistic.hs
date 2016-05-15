{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Statistic
Description : Statistic functions
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Statistic functions
-}
module Statistic
( Statistic (Counts)
, getCounts
)
where

import Data.Text (Text)
import Data.FileEmbed(embedStringFile)
import Database.SQLite3 ( Database, StepResult(Done, Row), prepare, finalize
                        , step, SQLData (SQLInteger), column
                        , ColumnIndex(ColumnIndex), SQLData
                        )

data Statistic = Counts deriving (Eq, Show)

getValue :: Database -> Text -> IO (Maybe SQLData)
getValue db req = do
    req' <- prepare db req

    result <- step req'
    value <- case result of
        Done -> return Nothing
        Row -> column req' (ColumnIndex 0) >>= return . Just

    finalize req'

    return value

getCounts :: Database -> IO (Maybe [Int])
getCounts db = do
    results <- sequence (getValue db <$> reqs)
    return $ sequence (sqlInt <$> results)
    where reqs =
            [ $(embedStringFile "src/Database/Stat/counts.sql")
            , $(embedStringFile "src/Database/Stat/checkedlinks.sql")
            , $(embedStringFile "src/Database/Stat/externallinks.sql")
            , $(embedStringFile "src/Database/Stat/htmlpages.sql")
            ]

          sqlInt (Just (SQLInteger i)) = Just (fromEnum i)
          sqlInt _ = Nothing

{-
httpcode=$(execdb "
    SELECT   '  - '
          || code.httpcode
          || ' '
          || code.description
          || ': '
          || COUNT(*)
    FROM     link, code
    WHERE    code.httpcode = link.httpcode
    GROUP BY link.httpcode
    ORDER BY link.httpcode ASC;
")

linktype=$(execdb "
    SELECT   '  - '
          || CASE WHEN contenttype = '' THEN '[unknown]' ELSE contenttype END
          || ': '
          || COUNT(*)
    FROM     link
    GROUP BY contenttype;
")

-}