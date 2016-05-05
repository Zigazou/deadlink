{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Curl (withCurlDo)
import Network.URI (parseURI)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO (stderr, hPutStrLn)
import Control.Monad (when, unless)

import Network.Link.Link (makeLink)
import Deadlink (deadlinkInit, deadlinkLoop)

main :: IO ()
main = do
    args <- getArgs

    -- Check presence of base link
    when (length args < 1) $ do
        hPutStrLn stderr "You must specify the base link"
        exitFailure

    -- Check for DB
    dbexist <- doesFileExist "deadlink.db"
    unless dbexist $ do
        hPutStrLn stderr "Cannot find deadlink.db"
        exitFailure

    case parseURI (head args) of
         Nothing -> hPutStrLn stderr "Unable to parse the base URI"
         Just uri -> withCurlDo $ do
             let baselink = makeLink uri

             deadlinkInit baselink
             deadlinkLoop baselink

