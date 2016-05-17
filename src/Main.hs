{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Network.Curl (withCurlDo)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (stderr, hPutStrLn)
import Control.Monad (unless)
import Data.Text (unpack)
import Data.FileEmbed(embedStringFile)

import Data.Link (makeLink)
import Database.DeadlinkDatabase (createDeadlinkDB)
import Database.SQLite3 (close)

import Database.MissingSQLite3 ( open2, SQLOpenFlag(SQLOpenReadOnly)
                               , SQLVFS (SQLVFSDefault)
                               )
import Deadlink (deadlinkInit, deadlinkLoop, getCurrentIteration)
import Commands (parseCommand, DeadlinkCommand (Create, Crawl, Help, Stat))
import Statistic ( Statistic(Counts, HttpCodes, ContentTypes, TopDeadlinks)
                 , getCounts, getHttpCodes, getContentTypes, getTopDeadlinks
                 )

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

doCommand (Stat dbname Counts) = do
    db <- open2 dbname [SQLOpenReadOnly] SQLVFSDefault
    allCounts <- getCounts db
    case allCounts of
         Just [counts, checked, external, htmlpage] -> do
             putStrLn $ "Total links: "    ++ show counts
             putStrLn $ "Checked links: "  ++ show checked
             putStrLn $ "External links: " ++ show external
             putStrLn $ "HTML pages: "     ++ show htmlpage
         _ -> hPutStrLn stderr "Unable to get stat" >> exitFailure
    close db

doCommand (Stat dbname HttpCodes) = do
    db <- open2 dbname [SQLOpenReadOnly] SQLVFSDefault
    getHttpCodes db >>= mapM_ (putStrLn . showCount)
    close db

    where showCount (code, codeCount, description) =
            show code ++ " - " ++ description ++ ": " ++ show codeCount

doCommand (Stat dbname TopDeadlinks) = do
    db <- open2 dbname [SQLOpenReadOnly] SQLVFSDefault
    getTopDeadlinks db >>= mapM_ putStrLn
    close db

doCommand (Stat dbname ContentTypes) = do
    db <- open2 dbname [SQLOpenReadOnly] SQLVFSDefault
    getContentTypes db >>= mapM_ (putStrLn . showTypes)
    close db

    where showTypes (mimetype, typeCount) = mimetype ++ ": " ++ show typeCount

doCommand Help = hPutStrLn stderr $(embedStringFile "src/help.txt")

main :: IO ()
main = do
    args <- getArgs

    case parseCommand args of
         Left parseError -> do
            hPutStrLn stderr (show parseError)
            exitFailure
         Right command -> doCommand command
