{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : LinkSQL
Description : Insert Link into an SQLite3 database
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Insert Link into an SQLite3 database
-}
module Database.LinkSQL
( insertLink
, updateLink
, getUncheckedLinks
, getUnparsedHTMLLinks
, getLastIteration
, remainingJob
, startTransaction
, endTransaction
)
where

import Data.Text (Text)
import Database.SQLite3 ( Database, StepResult(Done, Row), Statement
                        , prepare, finalize, step, SQLData, bindNamed
                        , column, ColumnIndex(ColumnIndex)
                        , SQLData (SQLInteger), exec
                        )

import Network.Link.Link
import Database.ToFromSQLite3

linkBinding :: Link -> [ (Text, SQLData ) ]
linkBinding = zip [":url", ":rc", ":ct", ":cd", ":pd"] . toSQLite3C

insertLink :: Database -> Int -> Link -> Link -> IO StepResult
insertLink db iteration parent link = do
    -- Insert the link
    reqLink <- prepare db "INSERT OR IGNORE INTO link \
                          \VALUES (:url, :rc, :ct, :cd, :pd);"
    bindNamed reqLink (linkBinding link)
    result <- step reqLink
    finalize reqLink

    -- Create the parent/children relationship
    reqParent <- prepare db "INSERT OR IGNORE INTO parent \
                            \VALUES (:parent, :child, :iteration);"

    let parentBinding = [ (":parent"   , toSQLite3S (linkURI parent))
                        , (":child"    , toSQLite3S (linkURI link))
                        , (":iteration", toSQLite3S iteration)
                        ]

    bindNamed reqParent parentBinding
    _ <- step reqParent
    finalize reqParent

    return result

updateLink :: Database -> Link -> IO StepResult
updateLink db link = do
    req <- prepare db "UPDATE link \
                      \SET httpcode = :rc, \
                          \contenttype = :ct, \
                          \checkdate = :cd, \
                          \parsedate = :pd \
                      \WHERE url = :url;"

    bindNamed req (linkBinding link)
    result <- step req
    finalize req
    return result

populate :: [Link] -> Statement -> IO [Link]
populate links req = do
    result <- step req
    case result of
        Done -> do
            finalize req
            return []
        Row -> do
            cols <- mapM (column req . ColumnIndex) [0 .. 4]
            let linkM = fromSQLite3C cols :: Maybe Link
            case linkM of
                Nothing -> populate links req
                Just link -> (link :) <$> populate links req

getUncheckedLinks :: Database -> IO [Link]
getUncheckedLinks db = do
    req <- prepare db "SELECT url, httpcode, contenttype, checkdate \
                      \FROM link \
                      \WHERE checkdate IS NULL \
                      \AND   parsedate IS NULL;"

    populate [] req

getUnparsedHTMLLinks :: Database -> Link -> IO [Link]
getUnparsedHTMLLinks db base = do
    req <- prepare db "SELECT url, httpcode, contenttype, checkdate \
                      \FROM link \
                      \WHERE contenttype LIKE 'text/html%' \
                      \AND   url LIKE :base \
                      \AND   parsedate IS NULL;"

    bindNamed req [ (":base", toSQLite3S $ url base ++ "%") ]

    populate [] req

remainingJob :: Database -> Link -> IO (Int, Int)
remainingJob db base = do
    -- Count the pages to parse
    reqPage <- prepare db "SELECT COUNT(*) \
                          \FROM   link \
                          \WHERE  contenttype LIKE 'text/html%' \
                          \AND    url LIKE :base \
                          \AND    parsedate IS NULL;"

    bindNamed reqPage [ (":base", toSQLite3S $ url base ++ "%") ]

    resultPage <- step reqPage
    pageCount <- case resultPage of
        Done -> return 0
        Row -> do
            (SQLInteger count) <- column reqPage (ColumnIndex 0)
            return $ fromEnum count
    finalize reqPage

    -- Count the links to check
    reqLink <- prepare db "SELECT COUNT(*) \
                          \FROM link \
                          \WHERE checkdate IS NULL \
                          \AND   parsedate IS NULL;"

    resultLink <- step reqLink
    linkCount <- case resultLink of
        Done -> return 0
        Row -> do
            (SQLInteger count) <- column reqLink (ColumnIndex 0)
            return $ fromEnum count
    finalize reqLink

    return (pageCount, linkCount)

-- | Starts a transaction in SQLite3
startTransaction :: Database -> IO ()
startTransaction db = exec db "BEGIN;"

-- | Ends a transaction in SQLite3
endTransaction :: Database -> IO ()
endTransaction db = exec db "END;"

getLastIteration :: Database -> IO Int
getLastIteration db = do
    -- Find the latest check date to get the current iteration
    reqIter <- prepare db "SELECT   iteration \
                          \FROM     parent \
                          \ORDER BY iteration DESC \
                          \LIMIT    1;"

    resultIter <- step reqIter
    iteration <- case resultIter of
        Done -> return 0
        Row -> do
            (SQLInteger iteration) <- column reqIter (ColumnIndex 0)
            return $ fromEnum iteration

    finalize reqIter

    return $ iteration + 1
