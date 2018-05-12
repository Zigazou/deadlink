{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : LinkChecker
Description : A link checker
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A link checker
-}
module Network.LinkChecker
( loadLinks
, verify
, parse
)
where

import qualified Data.Text as T

import Network.Curl ( CurlResponse, CurlOption
                    , curlGetResponse_, respHeaders, respStatus, respBody
                    )

import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (getCurrentTime)

import Data.ReferenceExtractor (findReferences)
import Data.Link
    ( Link ( UncheckedLink, ucURI
           , CheckedLink, chURI, chHTTPCode, chContentType, chCheckDate
           , ParsedLink, paURI, paHTTPCode, paContentType, paParseDate
           )
           , url, absolute, isReserved
    )

-- | Get all links from an HTML page
loadLinks :: [CurlOption] -> Link -> IO [Link]
loadLinks options baseLink = do
    resp <- curlGetResponse_ (url baseLink) options
    let locationM = case lookup ("Location" :: String) (respHeaders resp) of
                        Nothing -> Nothing
                        Just lTag -> absolute baseLink . T.strip . T.pack $ lTag

        (newBase, references) = findReferences (T.pack $ respBody resp)
        baseLink' = fromMaybe baseLink (absolute baseLink newBase)

    return $ catMaybes
        ( locationM
        : (absolute baseLink' <$> references)
        )

-- | Checks for a Link viability
verify :: [CurlOption] -> Link -> IO Link
verify options link@(UncheckedLink _)
    | isReserved link =  do
        checkDate <- getCurrentTime
        return CheckedLink { chURI         = ucURI link
                           , chHTTPCode    = 404
                           , chContentType = ""
                           , chCheckDate   = checkDate
                           }
    | otherwise = do
        resp <- curlGetResponse_ (url link) options :: IO CurlResponse

        checkDate <- getCurrentTime

        let httpCode = respStatus resp
            contentType = case lookup "Content-Type" (respHeaders resp) of
                            Nothing        -> ""
                            Just (' ':str) -> str
                            Just str       -> str

        return CheckedLink { chURI         = ucURI link
                           , chHTTPCode    = httpCode
                           , chContentType = contentType
                           , chCheckDate   = checkDate
                           }
verify _ link = return link

-- | Mark a link as parsed at current time
parse :: Link -> IO Link
parse (CheckedLink uri rc ct _) = do
    parseDate <- getCurrentTime
    return ParsedLink { paURI         = uri
                      , paHTTPCode    = rc
                      , paContentType = ct
                      , paParseDate   = parseDate
                      }
parse link = return link
