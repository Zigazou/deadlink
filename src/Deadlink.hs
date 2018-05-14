{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Deadlink
Description : DeadLink’s runtime
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Main DeadLink’s functions.
-}
module Deadlink
( deadlinkLoop
, deadlinkInit
, getCurrentIteration
)
where

import Network.URI.Text (nullURI)
import System.IO (stdout, hFlush)

import Data.Text (Text)
import Database.SQLite3 (open, close)

import Data.Link (Link, makeLink, pertinent)
import Network.LinkChecker (verify, parse, loadLinks)
import Database.LinkSQL ( getUncheckedLinks, getUnparsedHTMLLinks, updateLink
                        , insertLink, remainingJob, startTransaction
                        , endTransaction, getLastIteration
                        )

import Control.Monad (liftM)

import Settings (curlCheckOptions, curlLoadOptions)

getCurrentIteration :: Text -> IO Int
getCurrentIteration dbname = do
    db <- open dbname
    iteration <- getLastIteration db
    close db
    return iteration

checkPage :: Text -> Int -> Link -> IO Link
checkPage dbname iteration baseLink = do
    -- Load links from web page
    links <- liftM (filter (pertinent baseLink))
                   (loadLinks curlLoadOptions baseLink)

    db <- open dbname

    -- Insert pertinent links in the database
    actionPartition 50 links $ \links' -> do
        startTransaction db
        mapM_ (insertLink db iteration baseLink) links'
        endTransaction db

    -- Update current link
    baseLinkUpdated <- parse baseLink

    close db

    return baseLinkUpdated

-- | Initializes the database with the root element
deadlinkInit :: Text -> Link -> IO ()
deadlinkInit dbname link = do
    db <- open dbname
    _ <- insertLink db 0 (makeLink nullURI) link
    close db

-- | Group execution of actions
actionPartition :: Int -> [b] -> ([b] -> IO ()) -> IO ()
actionPartition _ [] _ = return ()
actionPartition nb list action = do
    action (take nb list)
    actionPartition nb (drop nb list) action

-- | An iteration consists of links checking followed by pages parsing
deadlinkIteration :: Text -> Int -> Link -> IO ()
deadlinkIteration dbname iteration base = do
    db <- open dbname

    -- Get unchecked links
    uncheckeds <- getUncheckedLinks db

    --putStr $ "Checking " ++ show (length uncheckeds) ++ " links"
    putStr $ "Checking many links"

    -- Update links states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much links must be recorded
    actionPartition 50 uncheckeds $ \list -> do
        linksToUpdate <- mapM ((tick '.' >>) . verify curlCheckOptions) list
        startTransaction db
        mapM_ (updateLink db) linksToUpdate
        endTransaction db
        tick '*'

    -- Check every unparsed HTML page
    unparseds <- getUnparsedHTMLLinks db base

    --putStr $ "\nParsing " ++ show (length unparseds) ++ " pages"
    putStr $ "\nParsing many pages"

    -- Update pages states. It works 50 links by 50 links to overcome a bug
    -- which appears when too much pages must be recorded
    actionPartition 50 unparseds $ \list -> do
        pagesToUpdate <- mapM ((tick '.' >>) . checkPage dbname iteration) list
        startTransaction db
        mapM_ (updateLink db) pagesToUpdate
        endTransaction db
        tick '*'

    close db

    putStr "\n"

    where tick c = putChar c >> hFlush stdout

-- | Loop again and again till there is no more links to check or page to
--   parse. It is the responsibility of the caller to call `withCurlDo` before
--   calling this function.
deadlinkLoop :: Text -> Int -> Link -> IO ()
deadlinkLoop _ 15 _ = putStrLn "16 iterations, I stop here!"
deadlinkLoop dbname iteration baselink = do
    putStrLn $ "Iteration " ++ show iteration

    db <- open dbname
    (pageCount, linkCount) <- remainingJob db baselink
    close db

    if pageCount == 0 && linkCount == 0
        then putStrLn "Finished!"
        else do
            deadlinkIteration dbname iteration baselink
            deadlinkLoop dbname (iteration + 1) baselink
