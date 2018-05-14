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
( Statistic (Counts, HttpCodes, ContentTypes, TopDeadlinks)
, getCounts
, getHttpCodes
, getContentTypes
, getTopDeadlinks
)
where

import Control.Monad (liftM)
import Data.Text (unpack)
import Data.FileEmbed(embedStringFile)
import Database.SQLite3 ( Database, StepResult(Done, Row), prepare, finalize
                        , step, SQLData (SQLInteger, SQLText), column
                        , ColumnIndex(ColumnIndex), SQLData, Statement
                        )

{-| All the Statistics that can be retrieved. -}
data Statistic = Counts
               | HttpCodes
               | ContentTypes
               | TopDeadlinks
               deriving (Eq, Show)

{-| Retrieve one value from a Statement.
    The first parameter is a function used to convert SQLData into any wanted
    data type, if possible.
-}
getValue :: (SQLData -> Maybe a) -> Statement -> IO (Maybe a)
getValue convert statement = do
    result <- step statement
    valueM <- case result of
        Done -> return Nothing
        Row -> liftM convert (column statement (ColumnIndex 0))
    finalize statement
    return valueM

{-| Retrieve a list of values from a Statement.
    Given a statement that retrieve one value, it will go on retrieve value
    after value until the statement has no more value to provide.
-}
getValues :: (Statement -> IO a) -> Statement -> IO [a]
getValues doOneValue statement = do
    result <- step statement
    case result of
        Done -> finalize statement >> return []
        Row -> do
            value <- doOneValue statement
            nextValues <- getValues doOneValue statement
            return (value:nextValues)

{-| Get various statistics from a Deadlink database.
    1. Count of links contained in the database
    2. Count of links that have already been checked
    3. Count of links pointing outside the base link
    4. Count of HTML pages encountered

    If any of the statistics cannot be retrieved, the function returns Nothing.
-}
getCounts :: Database -> IO (Maybe [Int])
getCounts db = liftM sequence (mapM doOneReq reqs)
    where
        reqs =
            [ $(embedStringFile "src/Database/Stat/counts.sql")
            , $(embedStringFile "src/Database/Stat/checkedlinks.sql")
            , $(embedStringFile "src/Database/Stat/externallinks.sql")
            , $(embedStringFile "src/Database/Stat/htmlpages.sql")
            ]

        sqlInt (SQLInteger i) = Just (fromEnum i)
        sqlInt _ = Nothing

        doOneReq = (>>= getValue sqlInt) . prepare db

{-| Get a list of HTTP codes encountered, the number of times it was encountered
    and a textual version of the HTTP code.
-}
getHttpCodes :: Database -> IO [(Int, Int, String)]
getHttpCodes db = do
    let req = $(embedStringFile "src/Database/Stat/httpcodes.sql")
    prepare db req >>= getValues doOneValue
    where doOneValue r = do
            (SQLInteger httpcode)  <- column r (ColumnIndex 0)
            (SQLText description)  <- column r (ColumnIndex 1)
            (SQLInteger codeCount) <- column r (ColumnIndex 2)
            return (fromEnum httpcode, fromEnum codeCount, unpack description)

{-| Get a list of content types encountered and the number of times it was
    encountered.
-}
getContentTypes :: Database -> IO [(String, Int)]
getContentTypes db = do
    let req = $(embedStringFile "src/Database/Stat/contenttypes.sql")
    prepare db req >>= getValues doOneValue
    where doOneValue r = do
            (SQLText mimetype)     <- column r (ColumnIndex 0)
            (SQLInteger typeCount) <- column r (ColumnIndex 1)
            return (unpack mimetype, fromEnum typeCount)

{-| Get a list of the most used dead links. -}
getTopDeadlinks :: Database -> IO [String]
getTopDeadlinks db = do
    let req = $(embedStringFile "src/Database/Stat/topdeadlinks.sql")
    prepare db req >>= getValues doOneValue
    where doOneValue r = do
            (SQLText deadlink) <- column r (ColumnIndex 0)
            return $ unpack deadlink
