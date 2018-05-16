{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
--------------------------------------------------------------------------------
-- |
--  Module      :  Network.URI.Text
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Graham Klyne <gk@ninebynine.org>
--  Stability   :  provisional
--  Portability :  portable
--
--  This is a Text-oriented version. Everything deprecated and not GHC related
--  has been removed.
--
--  This module defines functions for handling URIs.  It presents substantially
--  the same interface as the older GHC Network.URI module, but is implemented
--  using Parsec rather than a Regex library that is not available with Hugs.
--  The internal representation of URI has been changed so that URI strings are
--  more completely preserved when round-tripping to a URI value and back.
--
--  In addition, four methods are provided for parsing different
--  kinds of URI string (as noted in RFC3986):
--      'parseURI',
--      'parseURIReference',
--      'parseRelativeReference' and
--      'parseAbsoluteURI'.
--
--  Further, four methods are provided for classifying different
--  kinds of URI string (as noted in RFC3986):
--      'isURI',
--      'isURIReference',
--      'isRelativeReference' and
--      'isAbsoluteURI'.
--
--  The long-standing official reference for URI handling was RFC2396 [1],
--  as updated by RFC 2732 [2], but this was replaced by a new specification,
--  RFC3986 [3] in January 2005.  This latter specification has been used
--  as the primary reference for constructing the URI parser implemented
--  here, and it is intended that there is a direct relationship between
--  the syntax definition in that document and this parser implementation.
--
--  RFC 1808 [4] contains a number of test cases for relative URI handling.
--  Dan Connolly's Python module @uripath.py@ [5] also contains useful details
--  and test cases.
--
--  Some of the code has been copied from the previous GHC implementation,
--  but the parser is replaced with one that performs more complete
--  syntax checking of the URI itself, according to RFC3986 [3].
--
--  References
--
--  (1) <http://www.ietf.org/rfc/rfc2396.txt>
--
--  (2) <http://www.ietf.org/rfc/rfc2732.txt>
--
--  (3) <http://www.ietf.org/rfc/rfc3986.txt>
--
--  (4) <http://www.ietf.org/rfc/rfc1808.txt>
--
--  (5) <http://www.w3.org/2000/10/swap/uripath.py>
--
--------------------------------------------------------------------------------

module Network.URI.Text
    ( -- * The URI type
      URI(..)
    , URIAuth(..)
    , nullURI
      -- * Parsing
    , parseURI                  -- :: Text -> Maybe URI
    , parseURIReference         -- :: Text -> Maybe URI
    , parseRelativeReference    -- :: Text -> Maybe URI
    , parseAbsoluteURI          -- :: Text -> Maybe URI
      -- * Test for strings containing various kinds of URI
    , isURI
    , isURIReference
    , isRelativeReference
    , isAbsoluteURI
    , isIPv6address
    , isIPv4address
      -- * Relative URIs
    , relativeTo                -- :: URI -> URI -> Maybe URI
    , nonStrictRelativeTo       -- :: URI -> URI -> Maybe URI
    , relativeFrom              -- :: URI -> URI -> URI
      -- * Operations on URI strings
      -- | Support for putting strings into URI-friendly
      --   escaped format and getting them back again.
      --   This can't be done transparently in all cases, because certain
      --   characters have different meanings in different kinds of URI.
      --   The URI spec [3], section 2.4, indicates that all URI components
      --   should be escaped before they are assembled as a URI:
      --   \"Once produced, a URI is always in its percent-encoded form\"
    , uriToString               -- :: URI -> ShowS
    , isReserved, isUnreserved  -- :: Char -> Bool
    , isAllowedInURI, isUnescapedInURI  -- :: Char -> Bool
    , escapeURIChar             -- :: (Char->Bool) -> Char -> Text
    , escapeURIString           -- :: (Char->Bool) -> Text -> Text
    , unEscapeString            -- :: Text -> Text
    -- * URI Normalization functions
    , normalizeCase             -- :: Text -> Text
    , normalizeEscape           -- :: Text -> Text
    , normalizePathSegments     -- :: Text -> Text
    )
where

import Data.Text ( Text )
import qualified Data.Text as T

import Text.Parsec
    ( ParseError
    , parse, (<|>), (<?>), try
    , option, many, many1, count, notFollowedBy
    , char, satisfy, oneOf, string, eof
    , unexpected
    )

import Text.Parsec.Text ( GenParser )

import Data.Char( ord, chr, isHexDigit, toLower, toUpper, digitToInt )

import Numeric( showIntAtBase )

import Data.Typeable  ( Typeable )
import Data.Data      ( Data )

------------------------------------------------------------
--  The URI datatype
------------------------------------------------------------

-- |Represents a general universal resource identifier using
--  its component parts.
--
--  For example, for the URI
--
--  >   foo://anonymous@www.haskell.org:42/ghc?query#frag
--
--  the components are:
--
data URI = URI
    { uriScheme     :: Text             -- ^ @foo:@
    , uriAuthority  :: Maybe URIAuth    -- ^ @\/\/anonymous\@www.haskell.org:42@
    , uriPath       :: Text             -- ^ @\/ghc@
    , uriQuery      :: Text             -- ^ @?query@
    , uriFragment   :: Text             -- ^ @#frag@
    } deriving (Eq, Typeable, Data)

-- |Type for authority value within a URI
data URIAuth = URIAuth
    { uriUserInfo   :: Text             -- ^ @anonymous\@@
    , uriRegName    :: Text             -- ^ @www.haskell.org@
    , uriPort       :: Text             -- ^ @:42@
    } deriving (Eq, Typeable, Data, Show)

-- |Blank URI
nullURI :: URI
nullURI = URI
    { uriScheme     = T.empty
    , uriAuthority  = Nothing
    , uriPath       = T.empty
    , uriQuery      = T.empty
    , uriFragment   = T.empty
    }

--  URI as instance of Show.  Note that for security reasons, the default
--  behaviour is to suppress any userinfo field (see RFC3986, section 7.5).
--  This can be overridden by using uriToText directly with first
--  argument @id@ (noting that this returns a ShowS value rather than a string).
--
--  [[[Another design would be to embed the userinfo mapping function in
--  the URIAuth value, with the default value suppressing userinfo formatting,
--  but providing a function to return a new URI value with userinfo
--  data exposed by show.]]]
--
instance Show URI where
    showsPrec _ aUri = uriToString defaultUserInfoMap aUri

defaultUserInfoMap :: Text -> Text
defaultUserInfoMap uinf = T.append user newpass
    where
        (user, pass) = T.break (== ':') uinf
        newpass      = if T.null pass || (pass == "@") || (pass == ":@")
                       then pass
                       else ":...@"

------------------------------------------------------------
--  Parse a URI
------------------------------------------------------------

-- |Turn a string containing a URI into a 'URI'.
--  Returns 'Nothing' if the string is not a valid URI;
--  (an absolute URI with optional fragment identifier).
--
--  NOTE: this is different from the previous network.URI,
--  whose @parseURI@ function works like 'parseURIReference'
--  in this module.
--
parseURI :: Text -> Maybe URI
parseURI = parseURIAny uri

-- |Parse a URI reference to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid URI reference.
--  (an absolute or relative URI with optional fragment identifier).
--
parseURIReference :: Text -> Maybe URI
parseURIReference = parseURIAny uriReference

-- |Parse a relative URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid relative URI.
--  (a relative URI with optional fragment identifier).
--
parseRelativeReference :: Text -> Maybe URI
parseRelativeReference = parseURIAny relativeRef

-- |Parse an absolute URI to a 'URI' value.
--  Returns 'Nothing' if the string is not a valid absolute URI.
--  (an absolute URI without a fragment identifier).
--
parseAbsoluteURI :: Text -> Maybe URI
parseAbsoluteURI = parseURIAny absoluteURI

-- |Test if string contains a valid URI
--  (an absolute URI with optional fragment identifier).
--
isURI :: Text -> Bool
isURI = isValidParse uri

-- |Test if string contains a valid URI reference
--  (an absolute or relative URI with optional fragment identifier).
--
isURIReference :: Text -> Bool
isURIReference = isValidParse uriReference

-- |Test if string contains a valid relative URI
--  (a relative URI with optional fragment identifier).
--
isRelativeReference :: Text -> Bool
isRelativeReference = isValidParse relativeRef

-- |Test if string contains a valid absolute URI
--  (an absolute URI without a fragment identifier).
--
isAbsoluteURI :: Text -> Bool
isAbsoluteURI = isValidParse absoluteURI

-- |Test if string contains a valid IPv6 address
--
isIPv6address :: Text -> Bool
isIPv6address = isValidParse ipv6address

-- |Test if string contains a valid IPv4 address
--
isIPv4address :: Text -> Bool
isIPv4address = isValidParse ipv4address

--  Helper function for turning a string into a URI
--
parseURIAny :: URIParser URI -> Text -> Maybe URI
parseURIAny parser uristr = case parseAll parser "" uristr of
        Left  _ -> Nothing
        Right u -> Just u

--  Helper function to test a string match to a parser
--
isValidParse :: URIParser a -> Text -> Bool
isValidParse parser uristr = case parseAll parser "" uristr of
        Left  _ -> False
        Right _ -> True

parseAll :: URIParser a -> String -> Text -> Either ParseError a
parseAll parser filename uristr = parse newparser filename uristr
    where newparser = do
                res <- parser
                eof
                return res

------------------------------------------------------------
--  URI parser body based on Parsec elements and combinators
------------------------------------------------------------

--  Parser parser type.
--  Currently
type URIParser a = GenParser () a

--  RFC3986, section 2.1
--
--  Parse and return a 'pct-encoded' sequence
--
escaped :: URIParser Text
escaped = do
    _ <- char '%'
    h1 <- hexDigitChar
    h2 <- hexDigitChar
    return $ T.cons '%' (T.append h1 h2)

--  RFC3986, section 2.2
--
-- |Returns 'True' if the character is a \"reserved\" character in a
--  URI.  To include a literal instance of one of these characters in a
--  component of a URI, it must be escaped.
--
isReserved :: Char -> Bool
isReserved c = isGenDelims c || isSubDelims c

isGenDelims :: Char -> Bool
isGenDelims c = c `telem` ":/?#[]@"

isSubDelims :: Char -> Bool
isSubDelims c = c `telem` "!$&'()*+,;="

subDelims :: URIParser Text
subDelims = T.singleton <$> satisfy isSubDelims

telem :: Char -> Text -> Bool
telem needle haystack = T.findIndex (== needle) haystack /= Nothing

--  RFC3986, section 2.3
--
-- |Returns 'True' if the character is an \"unreserved\" character in
--  a URI.  These characters do not need to be escaped in a URI.  The
--  only characters allowed in a URI are either \"reserved\",
--  \"unreserved\", or an escape sequence (@%@ followed by two hex digits).
--
isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `telem` "-_.~")

unreservedChar :: URIParser Text
unreservedChar = T.singleton <$> satisfy isUnreserved

--  RFC3986, section 3
--
--   URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
--
--   hier-part   = "//" authority path-abempty
--               / path-abs
--               / path-rootless
--               / path-empty

uri :: URIParser URI
uri = do
    us <- try uscheme
    (ua,up) <- hierPart
    uq <- option "" ( char '?' >> uquery    )
    uf <- option "" ( char '#' >> ufragment )
    return $ URI
        { uriScheme    = us
        , uriAuthority = ua
        , uriPath      = up
        , uriQuery     = uq
        , uriFragment  = uf
        }

hierPart :: URIParser ((Maybe URIAuth), Text)
hierPart = withAuthority <|> absoluteWOAuth <|> rootLessWOAuth <|> nothingAtAll
    where
        withAuthority = do
            _ <- try (string "//")
            ua <- uauthority
            up <- pathAbEmpty
            return (ua, up)

        absoluteWOAuth = (Nothing ,) <$> pathAbs

        rootLessWOAuth = (Nothing ,) <$> pathRootLess

        nothingAtAll = return (Nothing, "")

--  RFC3986, section 3.1

uscheme :: URIParser Text
uscheme = do
    s <- oneThenMany alphaChar schemeChar
    _ <- char ':'
    return $ T.snoc s ':'

--  RFC3986, section 3.2

uauthority :: URIParser (Maybe URIAuth)
uauthority = do
    uu <- option "" (try userinfo)
    uh <- host
    up <- option "" port
    return $ Just $ URIAuth
        { uriUserInfo = uu
        , uriRegName  = uh
        , uriPort     = up
        }

--  RFC3986, section 3.2.1

userinfo :: URIParser Text
userinfo = do
    uu <- many (uchar ";:&=+$,")
    _ <- char '@'
    return (T.snoc (T.concat uu) '@')

--  RFC3986, section 3.2.2

host :: URIParser Text
host = ipLiteral <|> try ipv4address <|> regName

ipLiteral :: URIParser Text
ipLiteral = parseIpLiteral <?> "IP address literal"
    where parseIpLiteral = do
            _ <- char '['
            ua <- ( ipv6address <|> ipvFuture )
            _ <- char ']'
            return $ T.concat [ "[", ua, "]" ]

ipvFuture :: URIParser Text
ipvFuture = do
    _ <- char 'v'
    h <- hexDigitChar
    _ <- char '.'
    a <- many1 (satisfy isIpvFutureChar)
    return $ T.concat [ "c", h, ".", T.pack a ]

isIpvFutureChar :: Char -> Bool
isIpvFutureChar c = isUnreserved c || isSubDelims c || (c == ';')

ipv6address :: URIParser Text
ipv6address = try case1 <|> try case2 <|> try case3 <|> try case4
          <|> try case5 <|> try case6 <|> try case7 <|> try case8
          <|> try case9 <?> "IPv6 address"
    where
        case1 = do
            a2 <- count 6 h4c
            a3 <- ls32
            return $ T.append (T.concat a2) a3
        case2 = do
            _ <- string "::"
            a2 <- count 5 h4c
            a3 <- ls32
            return $ T.concat [ "::", T.concat a2, a3 ]
        case3 = do
            a1 <- opt_n_h4c_h4 0
            _ <- string "::"
            a2 <- count 4 h4c
            a3 <- ls32
            return $ T.concat [ a1, "::", T.concat a2, a3 ]
        case4 = do
            a1 <- opt_n_h4c_h4 1
            _ <- string "::"
            a2 <- count 3 h4c
            a3 <- ls32
            return $ T.concat [ a1, "::", T.concat a2, a3 ]
        case5 = do
            a1 <- opt_n_h4c_h4 2
            _ <- string "::"
            a2 <- count 2 h4c
            a3 <- ls32
            return $ T.concat [ a1, "::", T.concat a2, a3 ]
        case6 = do
            a1 <- opt_n_h4c_h4 3
            _ <- string "::"
            a2 <- h4c
            a3 <- ls32
            return $ T.concat [ a1, "::", a2, a3 ]
        case7 = do
            a1 <- opt_n_h4c_h4 4
            _ <- string "::"
            a3 <- ls32
            return $ T.concat [ a1, "::", a3 ]
        case8 = do
            a1 <- opt_n_h4c_h4 5
            _ <- string "::"
            a3 <- h4
            return $ T.concat [ a1, "::", a3 ]
        case9 = do
            a1 <- opt_n_h4c_h4 6
            _ <- string "::"
            return $ T.append a1 "::"

opt_n_h4c_h4 :: Int -> URIParser Text
opt_n_h4c_h4 n = option "" $ do
    a1 <- countMinMax 0 n h4c
    a2 <- h4
    return $ T.append a1 a2

ls32 :: URIParser Text
ls32 =  try parseLs32 <|> ipv4address
    where parseLs32 = h4c >>= \a1 -> h4 >>= \a2 -> return (T.append a1 a2)

h4c :: URIParser Text
h4c = try $ do
    a1 <- h4
    _ <- char ':'
    notFollowedBy (char ':')
    return $ T.snoc a1 ':'

h4 :: URIParser Text
h4 = countMinMax 1 4 hexDigitChar

ipv4address :: URIParser Text
ipv4address = do
    a1 <- decOctet
    _ <- char '.'
    a2 <- decOctet
    _ <- char '.'
    a3 <- decOctet
    _ <- char '.'
    a4 <- decOctet
    return $ T.concat [ a1, ".", a2, ".", a3, ".", a4 ]

decOctet :: URIParser Text
decOctet = do
    a1 <- countMinMax 1 3 digitChar
    if read (T.unpack a1) > (255 :: Integer)
        then fail "Decimal octet value too large"
        else return a1

regName :: URIParser Text
regName = countMinMax 0 255 ( unreservedChar <|> escaped <|> subDelims )
      <?> "Registered name"

--  RFC3986, section 3.2.3

port :: URIParser Text
port = do
    _ <- char ':'
    p <- many digitChar
    return (T.cons ':' (T.concat p))

--
--  RFC3986, section 3.3
--
--   path          = path-abempty    ; begins with "/" or is empty
--                 / path-abs        ; begins with "/" but not "//"
--                 / path-noscheme   ; begins with a non-colon segment
--                 / path-rootless   ; begins with a segment
--                 / path-empty      ; zero characters
--
--   path-abempty  = *( "/" segment )
--   path-abs      = "/" [ segment-nz *( "/" segment ) ]
--   path-noscheme = segment-nzc *( "/" segment )
--   path-rootless = segment-nz *( "/" segment )
--   path-empty    = 0<pchar>
--
--   segment       = *pchar
--   segment-nz    = 1*pchar
--   segment-nzc   = 1*( unreserved / pct-encoded / sub-delims / "@" )
--
--   pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

{-
upath :: URIParser Text
upath = pathAbEmpty
    <|> pathAbs
    <|> pathNoScheme
    <|> pathRootLess
    <|> pathEmpty
-}

pathAbEmpty :: URIParser Text
pathAbEmpty = T.concat <$> many slashSegment

pathAbs :: URIParser Text
pathAbs = char '/' >> T.cons '/' <$> option "" pathRootLess

pathNoScheme :: URIParser Text
pathNoScheme = do
    s1 <- segmentNzc
    ss <- many slashSegment
    return $ T.append s1 (T.concat ss)

pathRootLess :: URIParser Text
pathRootLess = do
    s1 <- segmentNz
    ss <- many slashSegment
    return $ T.append s1 (T.concat ss)

slashSegment :: URIParser Text
slashSegment = char '/' >> T.cons '/' <$> segment

segment :: URIParser Text
segment = T.concat <$> many pchar

segmentNz :: URIParser Text
segmentNz = T.concat <$> many1 pchar

segmentNzc :: URIParser Text
segmentNzc = T.concat <$> many1 (uchar "@")

pchar :: URIParser Text
pchar = uchar ":@"

-- helper function for pchar and friends
uchar :: String -> URIParser Text
uchar extras = unreservedChar <|> escaped <|> subDelims <|> oneOfExtras
    where oneOfExtras =  T.singleton <$> oneOf extras

--  RFC3986, section 3.4

uquery :: URIParser Text
uquery = do
    ss <- many $ uchar (":@" ++ "/?")
    return $ T.cons '?' (T.concat ss)

--  RFC3986, section 3.5

ufragment :: URIParser Text
ufragment = do
    ss <- many $ uchar (":@" ++ "/?")
    return $ T.cons '#' (T.concat ss)

--  Reference, Relative and Absolute URI forms
--
--  RFC3986, section 4.1

uriReference :: URIParser URI
uriReference = uri <|> relativeRef

--  RFC3986, section 4.2
--
--   relative-URI  = relative-part [ "?" query ] [ "#" fragment ]
--
--   relative-part = "//" authority path-abempty
--                 / path-abs
--                 / path-noscheme
--                 / path-empty

relativeRef :: URIParser URI
relativeRef = do
    notMatching uscheme
    (ua,up) <- relativePart
    uq <- option "" ( char '?' >> uquery )
    uf <- option "" ( char '#' >> ufragment )
    return $ URI
            { uriScheme    = T.empty
            , uriAuthority = ua
            , uriPath      = up
            , uriQuery     = uq
            , uriFragment  = uf
            }

relativePart :: URIParser ((Maybe URIAuth), Text)
relativePart = withAuthority 
           <|> (Nothing, ) <$> pathAbs
           <|> (Nothing, ) <$> pathNoScheme
           <|> return (Nothing, "")
    where withAuthority = do
            _ <- try (string "//")
            ua <- uauthority
            up <- pathAbEmpty
            return (ua, up)

--  RFC3986, section 4.3

absoluteURI :: URIParser URI
absoluteURI = do
    us <- uscheme
    (ua, up) <- hierPart
    uq <- option "" ( char '?' >> uquery )
    return $ URI
        { uriScheme    = us
        , uriAuthority = ua
        , uriPath      = up
        , uriQuery     = uq
        , uriFragment  = T.empty
        }

--  Imports from RFC 2234

    -- NOTE: can't use isAlphaNum etc. because these deal with ISO 8859
    -- (and possibly Unicode!) chars.
    -- [[[Above was a comment originally in GHC Network/URI.hs:
    --    when IRIs are introduced then most codepoints above 128(?) should
    --    be treated as unreserved, and higher codepoints for letters should
    --    certainly be allowed.
    -- ]]]

isAlphaChar :: Char -> Bool
isAlphaChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c = (c >= '0' && c <= '9')

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isHexDigitChar :: Char -> Bool
isHexDigitChar c = isHexDigit c

isSchemeChar :: Char -> Bool
isSchemeChar c = (isAlphaNumChar c) || (c `telem` "+-.")

alphaChar :: URIParser Text
alphaChar = T.singleton <$> satisfy isAlphaChar

digitChar :: URIParser Text
digitChar = T.singleton <$> satisfy isDigitChar

hexDigitChar :: URIParser Text
hexDigitChar = T.singleton <$> satisfy isHexDigitChar

schemeChar :: URIParser Text
schemeChar = T.singleton <$> satisfy isSchemeChar

--  Additional parser combinators for common patterns

oneThenMany :: GenParser t Text -> GenParser t Text -> GenParser t Text
oneThenMany p1 pr = do
    a1 <- p1
    ar <- many pr
    return $ T.append a1 (T.concat ar)

countMinMax :: Int -> Int -> GenParser t Text -> GenParser t Text
countMinMax m n parser | m > 0 = do
    a1 <- parser
    ar <- countMinMax (m - 1) (n - 1) parser
    return (T.append a1 ar)

countMinMax _ n _ | n <= 0 = return T.empty

countMinMax _ n parser = option T.empty $ do
    a1 <- parser
    ar <- countMinMax 0 (n-1) parser
    return (T.append a1 ar)

notMatching :: Show a => GenParser tok a -> GenParser tok ()
notMatching p = do { a <- try p ; unexpected (show a) } <|> return ()

------------------------------------------------------------
--  Reconstruct a URI string
------------------------------------------------------------
--
-- |Turn a 'URI' into a string.
--
--  Uses a supplied function to map the userinfo part of the URI.
--
--  The Show instance for URI uses a mapping that hides any password
--  that may be present in the URI.  Use this function with argument @id@
--  to preserve the password in the formatted output.
--
uriToString :: (Text->Text) -> URI -> ShowS
uriToString userinfomap URI { uriScheme=scheme
                            , uriAuthority=authority
                            , uriPath=path
                            , uriQuery=query
                            , uriFragment=fragment
                            } = (T.unpack scheme ++)
                              . uriAuthToString userinfomap authority
                              . (T.unpack path ++)
                              . (T.unpack query ++)
                              . (T.unpack fragment ++)

uriAuthToString :: (Text->Text) -> (Maybe URIAuth) -> ShowS
uriAuthToString _ Nothing   = id          -- shows ""
uriAuthToString userinfomap
        (Just URIAuth { uriUserInfo = uinfo
                      , uriRegName  = regname
                      , uriPort     = portNumber
                      } ) = ("//" ++)
                          . (if T.null uinfo
                                then id
                                else ((T.unpack $ userinfomap uinfo)++)
                            )
                          . (T.unpack regname ++)
                          . (T.unpack portNumber ++)

------------------------------------------------------------
--  Character classes
------------------------------------------------------------

-- | Returns 'True' if the character is allowed in a URI.
--
isAllowedInURI :: Char -> Bool
isAllowedInURI c = isReserved c || isUnreserved c || c == '%' -- escape char

-- | Returns 'True' if the character is allowed unescaped in a URI.
--
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

------------------------------------------------------------
--  Escape sequence handling
------------------------------------------------------------

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeURIChar :: (Char -> Bool) -> Char -> Text
escapeURIChar p c
    | p c       = T.singleton c
    | otherwise = T.cons '%' (T.pack (myShowHex (ord c) ""))
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 (toChrHex) n r of
            []  -> "00"
            [aDigit] -> ['0', aDigit]
            digits  -> digits
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> Text             -- ^ the string to process
    -> Text             -- ^ the resulting URI string
escapeURIString p s = T.concatMap (escapeURIChar p) s

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
infixr 5 :<

pattern b :< bs <- (T.uncons -> Just (b, bs))
pattern Empty   <- (T.uncons -> Nothing)

unEscapeString :: Text -> Text
unEscapeString Empty = T.empty
unEscapeString ('%' :< x1 :< x2 :< s) | isHexDigit x1 && isHexDigit x2 =
    T.cons (chr (digitToInt x1 * 16 + digitToInt x2))
           (unEscapeString s)
unEscapeString (c :< s) = T.cons c (unEscapeString s)
unEscapeString _ = T.empty

------------------------------------------------------------
-- Resolving a relative URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the value of the
--  first 'URI' interpreted as relative to the second 'URI'.
--  For example:
--
--  > "foo" `relativeTo` "http://bar.org/" = "http://bar.org/foo"
--  > "http:foo" `nonStrictRelativeTo` "http://bar.org/" = "http://bar.org/foo"
--
--  Algorithm from RFC3986 [3], section 5.2.2
--

nonStrictRelativeTo :: URI -> URI -> Maybe URI
nonStrictRelativeTo ref base = relativeTo ref' base
    where
        ref' = if uriScheme ref == uriScheme base
               then ref { uriScheme="" }
               else ref

mergePaths :: [Text] -> Text
mergePaths [] = T.empty
mergePaths [a] = T.dropWhile (== '/') a
mergePaths (a:b:cs) = T.concat
    [ T.dropWhileEnd (== '/') a
    , "/"
    , mergePaths (b:cs)
    ]

-- |Compute an absolute 'URI' for a supplied URI
--  relative to a given base.
relativeTo :: URI -> URI -> Maybe URI
relativeTo ref base
    | uriScheme ref /= "" =
        just_segments ref
    | uriAuthority ref /= Nothing =
        just_segments ref { uriScheme = uriScheme base }
    | uriPath ref /= "" =
        if (T.head (uriPath ref) == '/') then
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                }
        else
            just_segments ref
                { uriScheme    = uriScheme base
                , uriAuthority = uriAuthority base
                , uriPath      = mergeURIsToPath base ref
                }
    | uriQuery ref /= "" =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            }
    | otherwise =
        just_segments ref
            { uriScheme    = uriScheme base
            , uriAuthority = uriAuthority base
            , uriPath      = uriPath base
            , uriQuery     = uriQuery base
            }
    where
        just_segments u = Just $ u { uriPath = removeDotSegments (uriPath u) }
        mergeURIsToPath b r
            | uriAuthority b /= Nothing && T.null pb = mergePaths ["", pr]
            | otherwise = mergePaths [pb, pr]
            where
                (pb, pr) = (uriPath b, uriPath r)

--  Remove dot segments, but protect leading '/' character
removeDotSegments :: Text -> Text
removeDotSegments ('/' :< ps) = T.cons '/' (elimDots ps [])
removeDotSegments ps = elimDots ps []

--  Second arg accumulates segments processed so far in reverse order
elimDots :: Text -> [Text] -> Text
elimDots Empty [] = ""
elimDots Empty rs = T.concat (reverse rs)
elimDots ('.' :< '/' :< ps) rs = elimDots ps rs
elimDots ('.' :< Empty) rs = elimDots T.empty rs
elimDots ('.' :< '.' :< '/' :< ps) rs = elimDots ps (dropHead rs)
elimDots ('.' :< '.' :< Empty) rs = elimDots T.empty (dropHead rs)
elimDots ps rs = elimDots ps1 (r:rs)
    where
        (r,ps1) = nextSegment ps

--  Return tail of non-null list, otherwise return null list
dropHead :: [a] -> [a]
dropHead [] = []
dropHead (_:rs) = rs

--  Returns the next segment and the rest of the path from a path string.
--  Each segment ends with the next '/' or the end of string.
--
nextSegment :: Text -> (Text, Text)
nextSegment ps =
    case T.breakOn "/" ps of
        (r, '/' :< ps1) -> (T.snoc r '/', ps1)
        (r, _)       -> (r, T.empty)

--  Split last (name) segment from path, returning (path,name)
splitLast :: Text -> (Text, Text)
splitLast = T.breakOnEnd "/"

------------------------------------------------------------
-- Finding a URI relative to a base URI
------------------------------------------------------------

-- |Returns a new 'URI' which represents the relative location of
--  the first 'URI' with respect to the second 'URI'.  Thus, the
--  values supplied are expected to be absolute URIs, and the result
--  returned may be a relative URI.
--
--  Example:
--
--  > "http://example.com/Root/sub1/name2#frag"
--  >   `relativeFrom` "http://example.com/Root/sub2/name2#frag"
--  >   == "../sub1/name2#frag"
--
--  There is no single correct implementation of this function,
--  but any acceptable implementation must satisfy the following:
--
--  > (uabs `relativeFrom` ubase) `relativeTo` ubase == uabs
--
--  For any valid absolute URI.
--  (cf. <http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html>
--       <http://lists.w3.org/Archives/Public/uri/2003Jan/0005.html>)
--
relativeFrom :: URI -> URI -> URI
relativeFrom uabs base
    | diff uriScheme    uabs base = uabs
    | diff uriAuthority uabs base = uabs { uriScheme = "" }
    | diff uriPath      uabs base = uabs
        { uriScheme    = T.empty
        , uriAuthority = Nothing
        , uriPath      = relPathFrom (removeBodyDotSegments $ uriPath uabs)
                                     (removeBodyDotSegments $ uriPath base)
        }
    | diff uriQuery     uabs base = uabs
        { uriScheme    = T.empty
        , uriAuthority = Nothing
        , uriPath      = T.empty
        }
    | otherwise = uabs          -- Always carry fragment from uabs
        { uriScheme    = T.empty
        , uriAuthority = Nothing
        , uriPath      = T.empty
        , uriQuery     = T.empty
        }
    where
        diff :: Eq b => (a -> b) -> a -> a -> Bool
        diff sel u1 u2 = sel u1 /= sel u2
        -- Remove dot segments except the final segment
        removeBodyDotSegments p = T.append (removeDotSegments p1) p2
            where (p1,p2) = splitLast p

relPathFrom :: Text -> Text -> Text
relPathFrom Empty _ = "/"
relPathFrom pabs Empty = pabs
relPathFrom pabs base =                 -- Construct a relative path segments
    if sa1 == sb1                       -- if the paths share a leading segment
        then if (sa1 == "/")            -- other than a leading '/'
            then if (sa2 == sb2)
                then relPathFrom1 ra2 rb2
                else pabs
            else relPathFrom1 ra1 rb1
        else pabs
    where
        (sa1, ra1) = nextSegment pabs
        (sb1, rb1) = nextSegment base
        (sa2, ra2) = nextSegment ra1
        (sb2, rb2) = nextSegment rb1

--  relPathFrom1 strips off trailing names from the supplied paths,
--  and calls difPathFrom to find the relative path from base to
--  target
relPathFrom1 :: Text -> Text -> Text
relPathFrom1 pabs base = relName
    where
        (sa, na) = splitLast pabs
        (sb, nb) = splitLast base
        rp = relSegsFrom sa sb
        relName = if T.null rp then
                      if (na == nb) then ""
                      else if protect na then T.append "./" na
                                         else na
                  else
                      T.append rp na
        -- Precede name with some path if it is null or contains a ':'
        protect name = T.null name || T.findIndex (== ':') name /= Nothing

--  relSegsFrom discards any common leading segments from both paths,
--  then invokes difSegsFrom to calculate a relative path from the end
--  of the base path to the end of the target path.
--  The final name is handled separately, so this deals only with
--  "directory" segtments.
--
relSegsFrom :: Text -> Text -> Text
relSegsFrom Empty Empty = "" -- paths are identical
relSegsFrom sabs base =
    if sa1 == sb1
        then relSegsFrom ra1 rb1
        else difSegsFrom sabs base
    where
        (sa1,ra1) = nextSegment sabs
        (sb1,rb1) = nextSegment base

--  difSegsFrom calculates a path difference from base to target,
--  not including the final name at the end of the path
--  (i.e. results always ends with '/')
--
--  This function operates under the invariant that the supplied
--  value of sabs is the desired path relative to the beginning of
--  base.  Thus, when base is empty, the desired path has been found.
--
difSegsFrom :: Text -> Text -> Text
difSegsFrom sabs Empty = sabs
difSegsFrom sabs base = difSegsFrom (T.append "../" sabs)
                                    (snd $ nextSegment base)

------------------------------------------------------------
--  Other normalization functions
------------------------------------------------------------

-- |Case normalization; cf. RFC3986 section 6.2.2.1
--  NOTE:  authority case normalization is not performed
--
normalizeCase :: Text -> Text
normalizeCase uristr = ncScheme uristr
    where
        ncScheme (':' :< cs) = T.cons ':' (ncEscape cs)
        ncScheme (c :< cs) | isSchemeChar c = T.cons (toLower c) (ncScheme cs)
        ncScheme _ = ncEscape uristr -- no scheme present

        ncEscape ('%' :< h1 :< h2 :< cs) = T.concat [ "%"
                                                    , T.singleton $ toUpper h1
                                                    , T.singleton $ toUpper h2
                                                    , ncEscape cs
                                                    ]
        ncEscape (c :< cs) = T.cons c (ncEscape cs)
        ncEscape _ = T.empty

-- |Encoding normalization; cf. RFC3986 section 6.2.2.2
--
normalizeEscape :: Text -> Text
normalizeEscape ('%' :< h1 :< h2 :< cs)
    | isHexDigit h1 && isHexDigit h2 && isUnreserved escval =
        T.cons escval (normalizeEscape cs)
    where escval = chr (digitToInt h1 * 16 + digitToInt h2)
normalizeEscape (c :< cs) = T.cons c (normalizeEscape cs)
normalizeEscape _ = T.empty

-- |Path segment normalization; cf. RFC3986 section 6.2.2.4
--
normalizePathSegments :: Text -> Text
normalizePathSegments uristr = normstr juri
    where
        juri = parseURI uristr
        normstr Nothing  = uristr
        normstr (Just u) = T.pack $ show (normuri u)
        normuri u = u { uriPath = removeDotSegments (uriPath u) }

--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
