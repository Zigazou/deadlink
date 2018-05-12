{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Text as T
import Text.HTML.TagSoup (Tag(TagOpen), parseTags)
import Data.Maybe (catMaybes, fromMaybe)

tagToReference :: Tag T.Text -> Maybe T.Text
tagToReference (TagOpen tagName attrs)
    | tagName == "a" = lookup "href" attrs
    | tagName == "link" = lookup "href" attrs
    | tagName == "script" = lookup "src" attrs
    | otherwise = Nothing
tagToReference _ = Nothing

-- | Find base tag if it exists
findBase :: [Tag T.Text] -> T.Text
findBase (TagOpen "base" attrs:_) = fromMaybe "" (lookup "href" attrs)
findBase (_:tags)                 = findBase tags
findBase []                       = ""

-- | Given an HTML string, find all references to resources (links, scripts,
--   etc.)
findReferences :: T.Text -> (T.Text, [T.Text])
findReferences document =
    ( findBase tags
    , catMaybes (tagToReference <$> tags)
    )
    where tags = parseTags document
