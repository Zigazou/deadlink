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
, databaseFileName
, curlLoadOptions
, curlCheckOptions
)
where

import Data.Text (Text, pack)
import Network.Curl ( CurlOption ( CurlUserAgent, CurlTimeout
                                 , CurlFollowLocation, CurlNoBody
                                 , CurlFailOnError
                                 )
                    )

-- | The User Agent string sent to each web server while crawling
userAgent :: String
userAgent = "Zigazou's DeadLink finder"

-- | Database file
databaseFileName :: Text
databaseFileName = pack "deadlink.db"

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
curlCheckOptions = CurlNoBody True : curlLoadOptions
