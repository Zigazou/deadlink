{-# LANGUAGE OverloadedStrings #-}
module Deadlink
( deadlinkLoop
, deadlinkInit
, getCurrentIteration
)
where

import Network.URI (nullURI)
import System.IO (stdout, hFlush)

import Database.SQLite3 (open, close)

import Network.Link.Link (Link, makeLink, pertinent)
import Network.Link.LinkChecker (verify, parse, loadLinks)

import Database.LinkSQL ( getUncheckedLinks, getUnparsedHTMLLinks, updateLink
                        , insertLink, limitTransaction, remainingJob
                        , startTransaction, endTransaction, getLastIteration
                        )

import Control.Monad (liftM)

import Settings (databaseFileName)

getCurrentIteration :: IO Int
getCurrentIteration = do
    db <- open databaseFileName
    iteration <- getLastIteration db
    close db
    return iteration

checkPage :: Int -> Link -> IO Link
checkPage iteration baseLink = do
    -- Load links from web page
    links <- liftM (filter (pertinent baseLink)) (loadLinks baseLink)

    db <- open databaseFileName

    -- Insert pertinent links in the database
    limitTransaction db 50 (insertLink db iteration baseLink) links

    -- Update current link
    baseLinkUpdated <- parse baseLink

    close db

    return baseLinkUpdated

-- | Initializes the database with the root element
deadlinkInit :: Link -> IO ()
deadlinkInit link = do
    db <- open databaseFileName
    _ <- insertLink db 0 (makeLink nullURI) link
    close db

-- | Group execution of actions
actionPartition :: Int -> [b] -> ([b] -> IO ()) -> IO ()
actionPartition _ [] _ = return ()
actionPartition nb list action = do
    action (take nb list)
    actionPartition nb (drop nb list) action

-- | An iteration consists of links checking followed by pages parsing
deadlinkIteration :: Int -> Link -> IO ()
deadlinkIteration iteration base = do
    db <- open databaseFileName

    -- Get unchecked links
    uncheckeds <- getUncheckedLinks db

    putStr $ "Checking " ++ show (length uncheckeds) ++ " links"

    -- Update links states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much links must be recorded
    actionPartition 50 uncheckeds $ \list -> do
        linksToUpdate <- mapM (tick verify) list
        startTransaction db
        mapM_ (updateLink db) linksToUpdate
        endTransaction db

    -- Check every unparsed HTML page
    unparseds <- getUnparsedHTMLLinks db base

    putStr $ "\nParsing " ++ show (length unparseds) ++ " pages"

    -- Update pages states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much pages must be recorded
    actionPartition 50 unparseds $ \list -> do
        pagesToUpdate <- mapM (tick (checkPage iteration)) list
        startTransaction db
        mapM_ (updateLink db) pagesToUpdate
        endTransaction db

    close db

    putStr "\n"

    where tick action l = do
            putChar '.'
            --putStrLn (url l)
            hFlush stdout
            action l

-- | Loop again and again till there is no more links to check or page to
--   parse. It is the responsibility of the caller to call `withCurlDo` before
--   calling this function.
deadlinkLoop :: Int -> Link -> IO ()
deadlinkLoop 15 _ = putStrLn "16 iterations, I stop here!"
deadlinkLoop iteration baselink = do
    putStrLn $ "Iteration " ++ show iteration

    db <- open databaseFileName
    (pageCount, linkCount) <- remainingJob db baselink
    close db

    if pageCount == 0 && linkCount == 0
        then putStrLn "Finished!"
        else do
            deadlinkIteration iteration baselink
            deadlinkLoop (iteration + 1) baselink
