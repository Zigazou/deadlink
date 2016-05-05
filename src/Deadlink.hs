{-# LANGUAGE OverloadedStrings #-}
module Deadlink
( deadlinkLoop
, deadlinkInit
)
where

import Network.URI (nullURI)
import System.IO (stdout, hFlush)

import Database.SQLite3 (open, close)

import Network.Link.Link (Link, makeLink, pertinent)
import Network.Link.LinkChecker (verify, parse, loadLinks)

import Database.LinkSQL ( getUncheckedLinks, getUnparsedHTMLLinks, updateLink
                        , insertLink, limitTransaction, remainingJob
                        , startTransaction, endTransaction
                        )

import Control.Monad (liftM)

import Settings (databaseFileName)

checkPage :: Link -> IO Link
checkPage baseLink = do
    -- Load links from web page
    links <- liftM (filter (pertinent baseLink)) (loadLinks baseLink)

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

deadlinkIteration :: Link -> IO ()
deadlinkIteration base = do
    db <- open databaseFileName

    -- Get unchecked links
    uncheckeds <- getUncheckedLinks db

    putStr $ "Checking " ++ show (length uncheckeds) ++ " links"

    -- Update links states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much links must be recorded
    actionPartition 50 uncheckeds $ \list -> do
        linksToUpdate <- mapM ((tick >>) . verify) list
        startTransaction db
        mapM_ (updateLink db) linksToUpdate
        endTransaction db

    -- Check every unparsed HTML page
    unparseds <- getUnparsedHTMLLinks db base

    putStr $ "\nParsing " ++ show (length unparseds) ++ " pages"

    -- Update pages states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much pages must be recorded
    actionPartition 50 unparseds $ \list -> do
        pagesToUpdate <- mapM ((tick >>) . checkPage) list
        startTransaction db
        mapM_ (updateLink db) pagesToUpdate
        endTransaction db

    close db

    putStr "\n"

    where tick = do
            putChar '.'
            hFlush stdout

-- | Loop again and again till there is no more links to check or page to
--   parse. It is the responsibility of the caller to call `withCurlDo` before
--   calling this function.
deadlinkLoop :: Link -> IO ()
deadlinkLoop baselink = do
     db <- open databaseFileName
     (pageCount, linkCount) <- remainingJob db baselink
     close db

     putStrLn $ show pageCount ++ " remaining pages to parse"
     putStrLn $ show linkCount ++ " remaining links to check"

     if pageCount == 0 && linkCount == 0
        then putStrLn "Finished!"
        else deadlinkIteration baselink >> deadlinkLoop baselink

