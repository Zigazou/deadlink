{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Link
Description : A link!
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A link!
-}
module Data.Link
( Link ( UncheckedLink, ucURI
       , CheckedLink, chURI, chHTTPCode, chCURLCode, chContentType, chCheckDate
       , ParsedLink, paURI, paHTTPCode, paCURLCode, paContentType, paParseDate
       )
, linkURI
, makeLink
, absolute
, pertinent
, url
, isReserved
)
where

import qualified Data.Text as T
import Network.URI.Text
    ( URI(URI, uriScheme, uriAuthority, uriPath, uriFragment)
    , URIAuth(URIAuth)
    , parseURIReference, relativeTo, uriToString
    )
import Data.Time (UTCTime)

-- | A link
data Link = UncheckedLink { ucURI :: URI }
          | CheckedLink { chURI         :: URI
                        , chHTTPCode    :: Int
                        , chCURLCode    :: Int
                        , chContentType :: String
                        , chCheckDate   :: UTCTime
                        }
          | ParsedLink { paURI         :: URI
                       , paHTTPCode    :: Int
                       , paCURLCode    :: Int
                       , paContentType :: String
                       , paParseDate   :: UTCTime
                       }
          deriving (Show, Eq)

linkURI :: Link -> URI
linkURI (UncheckedLink uri)     = uri
linkURI (CheckedLink uri _ _ _ _) = uri
linkURI (ParsedLink uri _ _ _ _)  = uri

startswith :: T.Text -> T.Text -> Bool
startswith needle haystack
    | T.null needle = False
    | T.null haystack = False
    | T.length needle > T.length haystack = False
    | otherwise = needle == T.take (T.length needle) haystack

endswith :: T.Text -> T.Text -> Bool
endswith needle haystack
    | T.null needle = False
    | T.null haystack = False
    | T.length needle > T.length haystack = False
    | otherwise = needle == T.takeEnd (T.length needle) haystack

-- | Create a Link based on a URI. Discard URI fragment as it is not relevant
--   and will avoid unnecessary checks.
makeLink :: URI -> Link
makeLink uri = UncheckedLink uri { uriFragment = "" }

-- | Given a base URI, get an absolute URI from a String
absolute :: Link -> T.Text -> Maybe Link
absolute baseLink path = do
    pathURI <- parseURIReference path
    relURI <- relativeTo pathURI $ linkURI baseLink
    return $ makeLink relURI

-- | Given a base Link and a Link, tells if the URI is pertinent or should be
--   ignored.
pertinent :: Link -> Link -> Bool
pertinent baseLink link
    | not (startswith "http" $ uriScheme uri) = False
    | uriScheme uri    /= uriScheme base      = True
    | uriAuthority uri /= uriAuthority base   = True
    | uriPath uri      /= uriPath base        = True
    | otherwise                               = False
    where uri = linkURI link
          base = linkURI baseLink

-- | Transform a Link into a String
url :: Link -> String
url link = uriToString id (linkURI link) ""

-- | Tells if a link points to a reserved domain name
isReserved :: Link -> Bool
isReserved (UncheckedLink (URI _ (Just uriAuth) _ _ _)) =
    isReserved_ uriAuth
isReserved (CheckedLink (URI _ (Just uriAuth) _ _ _) _ _ _ _) =
    isReserved_ uriAuth
isReserved (ParsedLink (URI _ (Just uriAuth) _ _ _) _ _ _ _) =
    isReserved_ uriAuth
isReserved _ = False

isReserved_ :: URIAuth -> Bool
isReserved_ (URIAuth _ regName _)
    | domain rn "test"        = True
    | domain rn "example"     = True
    | domain rn "invalid"     = True
    | domain rn "localhost"   = True
    | domain rn "example.com" = True
    | domain rn "example.net" = True
    | domain rn "example.org" = True
    | otherwise               = False
    where domain str tl = str == tl || endswith tl (T.cons '.' str)
          rn = T.toLower regName
