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
import Database.SQLite3 (Database, open, close)

import Data.Link (Link, makeLink, pertinent)
import Network.LinkChecker (verify, parse, loadLinks)
import Database.LinkSQL ( getUncheckedLinks, getUnparsedHTMLLinks, updateLink
                        , insertLink, remainingJob, getLastIteration
                        )

import Control.Monad (liftM)

import Settings (curlCheckOptions, curlLoadOptions)

getCurrentIteration :: Text -> IO Int
getCurrentIteration dbname = do
    db <- open dbname
    iteration <- getLastIteration db
    close db
    return iteration

checkPage :: Database -> Int -> Link -> IO Link
checkPage db iteration baseLink = do
    -- Load links from web page
    links <- liftM (filter (pertinent baseLink))
                   (loadLinks curlLoadOptions baseLink)

    -- Insert pertinent links in the database
    actionPartition 50 links (mapM_ (insertLink db iteration baseLink))

    -- Update current link
    parse baseLink

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
    (uncheckedCount, uncheckeds) <- getUncheckedLinks db

    putStr $ "Checking " ++ show uncheckedCount ++ " links"

    -- Update links states. Operates 50 links by 50 links to save memory.
    actionPartition 50 uncheckeds $ \list -> do
        mapM (verify curlCheckOptions) list >>= mapM_ (updateLink db)
        tick '*'

    -- Check every unparsed HTML page
    (unparsedCount, unparseds) <- getUnparsedHTMLLinks db base

    putStr $ "\nParsing " ++ show unparsedCount ++ " pages"

    -- Update pages states. Operates 50 links by 50 links to save memory.
    actionPartition 50 unparseds $ \list -> do
        mapM (checkPage db iteration) list >>= mapM_ (updateLink db)
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
