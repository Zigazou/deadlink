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

import Data.Text (Text, pack)
import System.FilePath.Posix (isValid)
import Network.URI (parseURI, URI)

import Statistic (Statistic (Counts))

data DeadlinkCommand = Create Text
                     | Crawl Text URI
                     | Stat Text Statistic
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
validDB :: String -> Either CommandError Text
validDB "" = Left $ InvalidArg "Invalid empty database file name"
validDB (':':_) = Left $ InvalidArg "Invalid special database file name"
validDB dbname
    | isValid dbname = Right (pack dbname)
    | otherwise = Left $ InvalidArg "Invalid database file name"

{-| Verify a URI is valid -}
validURI :: String -> Either CommandError URI
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

parseCommand ["create", dbname] = do
    dbname' <- validDB dbname
    return $ Create dbname'

parseCommand ("create":_) = Left $ ArgNumber "create requires one argument"

parseCommand ["crawl", dbname, baseURI] = do
    dbname' <- validDB dbname
    baseURI' <- validURI baseURI
    return $ Crawl dbname' baseURI'

parseCommand ("crawl":_) = Left $ ArgNumber "crawl requires two arguments"

parseCommand [] = Left $ Usage "Usage: deadlink <command> [args]"

parseCommand ["stat", dbname, "counts"] = do
    dbname' <- validDB dbname
    return $ Stat dbname' Counts

parseCommand (command:_) = Left $ Unknown (
        "Unknown command: " ++ command
        ++ "\nValid commands are: help, create, crawl"
    )