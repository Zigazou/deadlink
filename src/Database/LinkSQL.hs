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
)
where

import System.IO.Unsafe (unsafeInterleaveIO)
import Data.Text (Text)
import Database.SQLite3 ( Database, StepResult(Done, Row), Statement
                        , prepare, finalize, step, SQLData, bindNamed
                        , column, ColumnIndex(ColumnIndex)
                        , SQLData (SQLInteger), exec
                        )

import Data.Link (Link, linkURI, url)
import Database.ToFromSQLite3

linkBinding :: Link -> [ (Text, SQLData ) ]
linkBinding = zip [":url", ":rc", ":cc", ":ct", ":cd", ":pd"] . toSQLite3C

insertLink :: Database -> Int -> Link -> Link -> IO StepResult
insertLink db iteration parent link = do
    -- Insert the link
    reqLink <- prepare db "INSERT OR IGNORE INTO link \
                          \VALUES (:url, :rc, :cc, :ct, :cd, :pd);"
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
                          \curlcode = :cc, \
                          \contenttype = :ct, \
                          \checkdate = :cd, \
                          \parsedate = :pd \
                      \WHERE url = :url;"

    bindNamed req (linkBinding link)
    result <- step req
    finalize req
    return result

populate :: Statement -> IO [Link]
populate req = do
    result <- step req
    case result of
        Done -> unsafeInterleaveIO (finalize req >> return [])
        Row -> do
            cols <- mapM (column req . ColumnIndex) [0 .. 5]
            let linkM = fromSQLite3C cols :: Maybe Link

            remainingLinks <- unsafeInterleaveIO (populate req)

            return $ case linkM of
                Nothing -> remainingLinks
                Just link -> link:remainingLinks

getUncheckedLinks :: Database -> IO (Int, [Link])
getUncheckedLinks db = do
    -- Create a temporary table to store the list of unchecked links allowing
    -- the origin table to be modified while read the temporary table.
    exec db "DROP TABLE IF EXISTS unchecked;"

    exec db "CREATE TEMPORARY TABLE unchecked AS \
            \SELECT url, httpcode, curlcode, contenttype, checkdate \
            \FROM link \
            \WHERE checkdate IS NULL \
            \AND   parsedate IS NULL;"

    -- Calculate number of unparsed links
    reqCount <- prepare db "SELECT COUNT(*) FROM unchecked;"
    resCount <- step reqCount

    rowCount <- case resCount of
                    Done -> return 0
                    Row -> do
                        (SQLInteger count) <- column reqCount (ColumnIndex 0)
                        return $ fromEnum count

    finalize reqCount

    -- Return the result
    if rowCount == 0
        then return (0, [])
        else do
            req <- prepare db "SELECT url, \
                                     \httpcode, \
                                     \curlcode, \
                                     \contenttype, \
                                     \checkdate \
                              \FROM unchecked;"

            list <- populate req
            return (rowCount, list)

getUnparsedHTMLLinks :: Database -> Link -> IO (Int, [Link])
getUnparsedHTMLLinks db base = do
    -- Create a temporary table to store the list of unparsed links allowing
    -- the origin table to be modified while read the temporary table.
    exec db "DROP TABLE IF EXISTS unparsed;"
    
    cre <- prepare db "CREATE TEMPORARY TABLE unparsed AS \
                      \SELECT url, httpcode, curlcode, contenttype, checkdate \
                      \FROM link \
                      \WHERE contenttype LIKE 'text/html%' \
                      \AND   url LIKE :base \
                      \AND   parsedate IS NULL;"

    bindNamed cre [ (":base", toSQLite3S $ url base ++ "%") ]

    _ <- step cre
    
    finalize cre

    -- Calculate number of unparsed links
    reqCount <- prepare db "SELECT COUNT(*) FROM unparsed;"
    resCount <- step reqCount

    rowCount <- case resCount of
                    Done -> return 0
                    Row -> do
                        (SQLInteger count) <- column reqCount (ColumnIndex 0)
                        return $ fromEnum count

    finalize reqCount

    -- Return the result
    if rowCount == 0
        then return (0, [])
        else do
            req <- prepare db "SELECT url, \
                                     \httpcode, \
                                     \curlcode, \
                                     \contenttype, \
                                     \checkdate \
                              \FROM unparsed;"

            list <- populate req
            return (rowCount, list)

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
