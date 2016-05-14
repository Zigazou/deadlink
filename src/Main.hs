{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Curl (withCurlDo)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless)
import Data.Text (unpack)

import Data.Link (makeLink)
import Database.DeadlinkDatabase (createDeadlinkDB)

import Deadlink (deadlinkInit, deadlinkLoop, getCurrentIteration)
import Commands (parseCommand, DeadlinkCommand (Create, Crawl, Help))

{-| Execute a Deadlink command as parsed by the `parseCommand` function -}
doCommand :: DeadlinkCommand -> IO ()
doCommand (Create dbname) = createDeadlinkDB dbname

doCommand (Crawl dbname baseURI) = do
    -- Check for DB
    dbexist <- doesFileExist (unpack dbname)
    unless dbexist $ do
        hPutStrLn stderr ("Cannot read " ++ unpack dbname)
        exitFailure

    withCurlDo $ do
        let baselink = makeLink baseURI

        iteration <- getCurrentIteration dbname

        deadlinkInit dbname baselink
        deadlinkLoop dbname iteration baselink

doCommand Help = do
    hPutStrLn stderr "Deadlink help"
    hPutStrLn stderr "Usage: deadlink <command> [args]"
    hPutStrLn stderr "Commands:"
    hPutStrLn stderr "- create <dbname> -> create the database structure"
    hPutStrLn stderr "- crawl <dbname> <baseuri> -> crawl a website"
    hPutStrLn stderr "- help -> show this help"

main :: IO ()
main = do
    args <- getArgs

    case parseCommand args of
         Left parseError -> do
            hPutStrLn stderr (show parseError)
            exitFailure
         Right command -> doCommand command
