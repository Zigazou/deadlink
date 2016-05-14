{-|
Module      : Settings
Description : DeadLink’s settings
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module simply holds settings for the DeadLink application.
-}
module Settings
( userAgent
, curlLoadOptions
, curlCheckOptions
)
where

import Network.Curl ( CurlOption ( CurlUserAgent, CurlTimeout
                                 , CurlFollowLocation, CurlRange
                                 , CurlFailOnError
                                 )
                    )

-- | The User Agent string sent to each web server while crawling
userAgent :: String
userAgent = "Zigazou's DeadLink finder"

-- | Curl options used when loading content
curlLoadOptions :: [CurlOption]
curlLoadOptions =
    [ CurlUserAgent userAgent
    , CurlTimeout 10
    , CurlFollowLocation False
    , CurlFailOnError False
    ]

-- | Curl options used when checking content
curlCheckOptions :: [CurlOption]
curlCheckOptions = CurlRange "0-255"
                 : curlLoadOptions
