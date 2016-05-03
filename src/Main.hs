{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Curl (curl_global_init)
import Network.URI (parseURI, nullURI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (stderr, hFlush, hPutChar, hPutStr, hPutStrLn)
import Control.Monad (when)

import Database.SQLite3

import Network.Link.Link (Link, makeLink, pertinent)
import Network.Link.LinkChecker (verify, parse, loadLinks)

import Database.LinkSQL ( getUncheckedLinks, getUnparsedHTMLLinks, updateLink
                        , insertLink, limitTransaction, remainingJob
                        , startTransaction, endTransaction
                        )

import Settings (databaseFileName)

checkPage :: Link -> IO Link
checkPage baseLink = do
    -- Load links from web page
    links <- loadLinks baseLink >>= return . filter (pertinent baseLink)

    db <- open databaseFileName

    -- Insert pertinent links in the database
    limitTransaction db 50 (insertLink db baseLink) links

    -- Update current link
    baseLinkUpdated <- parse baseLink

    close db

    return baseLinkUpdated

deadlinkInit :: Link -> IO ()
deadlinkInit link = do
    db <- open databaseFileName
    _ <- insertLink db (makeLink nullURI) link
    close db

actionPartition :: Int -> [b] -> ([b] -> IO ()) -> IO ()
actionPartition _ [] _ = return ()
actionPartition nb list action = do
    action (take nb list)
    actionPartition nb (drop nb list) action

deadlinkLoop :: Link -> IO ()
deadlinkLoop base = do
    _ <- curl_global_init 3
    db <- open databaseFileName

    -- Get unchecked links
    uncheckeds <- getUncheckedLinks db

    hPutStr stderr $ "Checking " ++ show (length uncheckeds) ++ " links"

    -- Update links states
    actionPartition 50 uncheckeds $ \list -> do
        linksToUpdate <- mapM (\l -> tick >> verify l) list
        startTransaction db
        mapM_ (updateLink db) linksToUpdate
        endTransaction db

    -- Check every unparsed HTML page
    unparseds <- getUnparsedHTMLLinks db base

    hPutStr stderr $ "\nParsing " ++ show (length unparseds) ++ " pages"

    actionPartition 50 unparseds $ \list -> do
        pagesToUpdate <- mapM (\l -> tick >> checkPage l) list
        startTransaction db
        mapM_ (updateLink db) pagesToUpdate
        endTransaction db

    close db

    hPutStr stderr "\n"

    where tick = do
            hPutChar stderr '.'
            hFlush stderr

main :: IO ()
main = do
    args <- getArgs

    -- Check presence of base link
    when (length args < 1) $ do
        hPutStrLn stderr "You must specify the base link"
        exitFailure

    -- Check for DB
    dbexist <- doesFileExist "deadlink.db"
    when (not dbexist) $ do
        hPutStrLn stderr "Cannot find deadlink.db"
        exitFailure

    case parseURI (head args) of
         Nothing -> hPutStrLn stderr "Unable to parse the base URI"
         Just uri -> do
             let baselink = makeLink uri

             deadlinkInit baselink
             deadlinkLoop baselink
             db <- open databaseFileName
             (pageCount, linkCount) <- remainingJob db baselink
             close db

             hPutStr stderr $ show pageCount ++ " remaining pages to parse\n"
             hPutStr stderr $ show linkCount ++ " remaining links to check\n"
