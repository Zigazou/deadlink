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
module Network.Link.LinkChecker
( loadLinks
, verify
, parse
)
where

import Network.Curl ( CurlResponse, curlGetString, curlGetResponse_
                    , respHeaders, respStatus
                    )

import Data.Maybe (catMaybes)
import Data.Time (getCurrentTime)

import Data.ReferenceExtractor (findReferences)
import Settings (curlCheckOptions, curlLoadOptions)
import Network.Link.Link
    ( Link ( UncheckedLink, ucURI
           , CheckedLink, chURI, chHTTPCode, chContentType, chCheckDate
           , ParsedLink, paURI, paHTTPCode, paContentType, paParseDate
           )
           , url, absolute, isReserved
    )

-- | Get all links from an HTML page
loadLinks :: Link -> IO [Link]
loadLinks baseLink = do
    (_, content) <- curlGetString (url baseLink) curlLoadOptions
    return $ catMaybes (absolute baseLink <$> findReferences content)

-- | Checks for a Link viability
verify :: Link -> IO Link
verify link@(UncheckedLink _)
    | isReserved link =  do
        checkDate <- getCurrentTime
        return $ CheckedLink { chURI = ucURI link
                            , chHTTPCode = 404
                            , chContentType = ""
                            , chCheckDate = checkDate
                            }
    | otherwise = do
        resp <- curlGetResponse_ (url link) curlCheckOptions :: IO CurlResponse

        checkDate <- getCurrentTime

        let httpCode = respStatus resp
            contentType = case lookup "Content-Type" (respHeaders resp) of
                            Nothing -> ""
                            Just (' ':str) -> str
                            Just str -> str

        return $ CheckedLink { chURI = ucURI link
                            , chHTTPCode = httpCode
                            , chContentType = contentType
                            , chCheckDate = checkDate
                            }
verify link = return link

parse :: Link -> IO Link
parse (CheckedLink uri rc ct _) = do
    parseDate <- getCurrentTime
    return $ ParsedLink { paURI = uri
                        , paHTTPCode = rc
                        , paContentType = ct
                        , paParseDate = parseDate
                        }
parse link = return link

