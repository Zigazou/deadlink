{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Commands
Description : DeadLink’s commands parser
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Deadlink’s commands parser
-}
module Commands
( DeadlinkCommand(Create, Crawl, Help, Stat)
, CommandError(Unknown, ArgNumber, Usage)
, parseCommand
)
where

import Control.Monad (liftM, liftM2)
import qualified Data.Text as T
import System.FilePath.Posix (isValid)
import Network.URI.Text (parseURI, URI)

import Statistic (Statistic (Counts, HttpCodes, ContentTypes, TopDeadlinks))

data DeadlinkCommand = Create T.Text
                     | Crawl T.Text URI
                     | Stat T.Text Statistic
                     | Help
                     deriving (Eq, Show)

data CommandError = Unknown String
                  | ArgNumber String
                  | Usage String
                  | InvalidArg String
                  deriving (Eq, Show)

maybeToEither :: Maybe a -> b -> Either b a
maybeToEither Nothing b = Left b
maybeToEither (Just a) _ = Right a

{-| Verify a database name is a valid name
    It must be a valid file path but it also must not start with ':'
    because it has special meaning for SQLite.
-}
validDB :: T.Text -> Either CommandError T.Text
validDB "" = Left $ InvalidArg "Invalid empty database file name"
validDB dbname
    | T.head dbname == ':' = Left
                           $ InvalidArg "Invalid special database file name"
    | isValid (T.unpack dbname) = Right dbname
    | otherwise = Left $ InvalidArg "Invalid database file name"

validStatType :: String -> Either CommandError Statistic
validStatType "counts" = Right Counts
validStatType "httpcodes" = Right HttpCodes
validStatType "contenttypes" = Right ContentTypes
validStatType "topdeadlinks" = Right TopDeadlinks
validStatType _ = Left $ InvalidArg "Invalid statistic type"

{-| Verify a URI is valid -}
validURI :: T.Text -> Either CommandError URI
validURI path = maybeToEither (parseURI path) (InvalidArg "Invalid URI")

{-|
Parse a DeadLink’s command

It either returns an DeadLink’s command or an error.
The parser checks that the parameters are correct while being pure. It means
that, for example, file names are checked for their validity but not for their
existence in the file system.
-}
parseCommand :: [String] -> Either CommandError DeadlinkCommand
parseCommand ["help"] = Right Help
parseCommand ["create", dbname] = liftM Create (validDB (T.pack dbname))
parseCommand ("create":_) = Left $ ArgNumber "create requires one argument"
parseCommand ["crawl", d, b] =
    liftM2 Crawl (validDB (T.pack d)) (validURI (T.pack b))
parseCommand ("crawl":_) = Left $ ArgNumber "crawl requires two arguments"
parseCommand [] = Left $ Usage "Usage: deadlink <command> [args]"
parseCommand ["stat", d, s] =
    liftM2 Stat (validDB (T.pack d)) (validStatType s)
parseCommand (command:_) = Left $ Unknown (
        "Unknown command: " ++ command
        ++ "\nValid commands are: help, create, crawl, stat"
    )
