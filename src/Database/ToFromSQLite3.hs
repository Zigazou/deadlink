{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : ToSQLite3
Description : Class for type conversion to SQLite3
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

Class for type conversion to SQLite3
-}
module Database.ToFromSQLite3
( ToFromSQLite3Simple (toSQLite3S, fromSQLite3S)
, ToFromSQLite3Composite (toSQLite3C, fromSQLite3C)
)
where

import Database.SQLite3 (SQLData (SQLInteger, SQLText, SQLNull))
import Data.Time ( UTCTime, formatTime, defaultTimeLocale, iso8601DateFormat
                 , parseTimeM
                 )
import Data.Text (pack, unpack)
import Network.URI (URI, parseURI, uriToString)

import Data.Link (Link (UncheckedLink, ParsedLink, CheckedLink))

class ToFromSQLite3Simple t where
    toSQLite3S :: t -> SQLData
    fromSQLite3S :: SQLData -> Maybe t

instance ToFromSQLite3Simple String where
    toSQLite3S = SQLText . pack

    fromSQLite3S (SQLText t) = Just (unpack t)
    fromSQLite3S _ = Nothing

instance ToFromSQLite3Simple Int where
    toSQLite3S = SQLInteger . toEnum

    fromSQLite3S (SQLInteger i) = Just . fromEnum $ i
    fromSQLite3S _ = Nothing

instance ToFromSQLite3Simple URI where
    toSQLite3S uri = SQLText . pack $ uriToString id uri ""

    fromSQLite3S (SQLText t) = parseURI (unpack t)
    fromSQLite3S _ = Nothing

instance ToFromSQLite3Simple UTCTime where
    toSQLite3S date = SQLText . pack $ fmtDate
        where fmtDate = formatTime  defaultTimeLocale
                                    (iso8601DateFormat $ Just "%H:%M:%S")
                                    date

    fromSQLite3S (SQLText t) = parseTimeM True
                                          defaultTimeLocale
                                          (iso8601DateFormat $ Just "%H:%M:%S")
                                          (unpack t)

    fromSQLite3S _ = Nothing

class ToFromSQLite3Composite t where
    toSQLite3C :: t -> [SQLData]
    fromSQLite3C :: [SQLData] -> Maybe t

instance ToFromSQLite3Composite Link where
    toSQLite3C (UncheckedLink uri) = [ toSQLite3S uri
                                     , SQLNull
                                     , SQLNull
                                     , SQLNull
                                     , SQLNull
                                     ]

    toSQLite3C (CheckedLink uri rc ct cd) = [ toSQLite3S uri
                                            , toSQLite3S rc
                                            , toSQLite3S ct
                                            , toSQLite3S cd
                                            , SQLNull
                                            ]

    toSQLite3C (ParsedLink uri rc ct pd) = [ toSQLite3S uri
                                           , toSQLite3S rc
                                           , toSQLite3S ct
                                           , SQLNull
                                           , toSQLite3S pd
                                           ]

    fromSQLite3C [urls, rcs, cts, _, SQLText pds] = do
        uri <- fromSQLite3S urls
        rc <- fromSQLite3S rcs
        ct <- fromSQLite3S cts
        pd <- fromSQLite3S (SQLText pds)
        return $ ParsedLink uri rc ct pd

    fromSQLite3C [urls, rcs, cts, SQLText cds, _] = do
        uri <- fromSQLite3S urls
        rc <- fromSQLite3S rcs
        ct <- fromSQLite3S cts
        cd <- fromSQLite3S (SQLText cds)
        return $ CheckedLink uri rc ct cd

    fromSQLite3C [urls, _, _, _, _] = do
        uri <- fromSQLite3S urls
        return $ UncheckedLink uri

    fromSQLite3C _ = Nothing
