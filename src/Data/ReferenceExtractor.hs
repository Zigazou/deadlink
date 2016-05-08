{-|
Module      : ReferenceExtractor
Description : Extract links from a, link etc. tag from HTML
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

A link extractor
-}
module Data.ReferenceExtractor (findReferences) where

import Text.HTML.TagSoup (Tag(TagOpen), parseTags)
import Data.Maybe (catMaybes, fromMaybe)

tagToReference :: Tag String -> Maybe String
tagToReference (TagOpen "a" attrs) = lookup "href" attrs
tagToReference (TagOpen "link" attrs) = lookup "href" attrs
tagToReference (TagOpen "script" attrs) = lookup "src" attrs
tagToReference _ = Nothing

-- | Find base tag if it exists
findBase :: [Tag String] -> String
findBase (TagOpen "base" attrs:_) = fromMaybe "" (lookup "href" attrs)
findBase (_:tags) = findBase tags
findBase [] = ""

-- | Given an HTML string, find all references to resources (links, scripts,
--   etc.)
findReferences :: String -> (String, [String])
findReferences document =
    ( findBase tags
    , catMaybes (tagToReference <$> tags)
    )
    where tags = parseTags document
