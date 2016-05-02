{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Curl (withCurlDo)
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
                        )

checkPage :: Database -> Link -> IO Link
checkPage db baseLink = do
    -- Load links from web page
    links <- loadLinks baseLink >>= return . filter (pertinent baseLink)

    -- Insert pertinent links in the database
    limitTransaction db 50 (insertLink db baseLink) links

    -- Update current link
    parse baseLink

deadlinkInit :: Database -> Link -> IO ()
deadlinkInit db link = insertLink db (makeLink nullURI) link >> return ()

deadlinkLoop :: Database -> Link -> IO ()
deadlinkLoop db base = withCurlDo $ do
    -- Get unchecked links
    uncheckeds <- getUncheckedLinks db

    hPutStr stderr $ "Checking " ++ show (length uncheckeds) ++ " links"

    -- Update links states
    linksToUpdate <- mapM (\l -> tick >> verify l) uncheckeds
    limitTransaction db 50 (updateLink db) linksToUpdate

    -- Check every unparsed HTML page
    unparseds <- getUnparsedHTMLLinks db base

    hPutStr stderr $ "\nParsing " ++ show (length unparseds) ++ " pages"

    pagesToUpdate <- mapM (\l -> tick >> checkPage db l) unparseds
    limitTransaction db 50 (updateLink db) pagesToUpdate

    hPutStr stderr "\n"

    where tick = do
            exec db "select 1 + 2;"
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
             db <- open "deadlink.db"

             deadlinkInit db baselink
             deadlinkLoop db baselink
             (pageCount, linkCount) <- remainingJob db baselink

             hPutStr stderr $ show pageCount ++ " remaining pages to parse\n"
             hPutStr stderr $ show linkCount ++ " remaining links to check\n"

             close db
