{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MissingSQLite3
Description : Defines the open2 SQLite3 missing function
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The package direct-sqlite3 does not define the open2 SQLite3 function. This
function allows, among other things, to set the flags defining how the open
function must behave. For example, it allows you to open an existing database
and not creating it if it doesn’t exist as is the case for the open function.

MissingSQLite3 defines the open2 SQLite3 function.

Since it is not part of the original package, some functions were copied from
direct-sqlite3 into MissingSQLite3 because these functions were not exported.
-}
module Database.MissingSQLite3
( open2
, SQLOpenFlag (..)
, SQLVFS (..)
) where

import Control.Exception (throwIO)
import Database.SQLite3.Bindings.Types ( CDatabase, CError (CError)
                                       , decodeError
                                       )
import Database.SQLite3.Direct ( Utf8 (Utf8), Database (Database), errmsg
                               , close, Statement
                               )
import qualified Database.SQLite3.Direct as Direct
import Database.SQLite3 ( Error, SQLError (SQLError), sqlError
                        , sqlErrorDetails, sqlErrorContext
                        )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

import Foreign
import Foreign.C

data SQLVFS = SQLVFSDefault
            | SQLVFSUnix
            | SQLVFSUnixDotFile
            | SQLVFSUnixExcl
            | SQLVFSUnixNone
            | SQLVFSUnixNamedSem
            deriving (Eq, Show)

toMaybeText :: SQLVFS -> Maybe Text
toMaybeText SQLVFSDefault = Nothing
toMaybeText SQLVFSUnix = Just "unix"
toMaybeText SQLVFSUnixDotFile = Just "unix-dotfile"
toMaybeText SQLVFSUnixExcl = Just "unix-excl"
toMaybeText SQLVFSUnixNone = Just "unix-none"
toMaybeText SQLVFSUnixNamedSem = Just "unix-namedsem"

data SQLOpenFlag = SQLOpenReadOnly      -- Ok for sqlite3_open2()
                 | SQLOpenReadWrite     -- Ok for sqlite3_open2()
                 | SQLOpenCreate        -- Ok for sqlite3_open2()
                 | SQLOpenDeleteOnClose -- VFS only
                 | SQLOpenExclusive     -- VFS only
                 | SQLOpenAutoProxy     -- VFS only
                 | SQLOpenURI           -- Ok for sqlite3_open2()
                 | SQLOpenMemory        -- Ok for sqlite3_open2()
                 | SQLOpenMainDB        -- VFS only
                 | SQLOpenTempDB        -- VFS only
                 | SQLOpenTransientDB   -- VFS only
                 | SQLOpenMainJournal   -- VFS only
                 | SQLOpenTempJournal   -- VFS only
                 | SQLOpenSubJournal    -- VFS only
                 | SQLOpenMasterJournal -- VFS only
                 | SQLOpenNoMutex       -- Ok for sqlite3_open2()
                 | SQLOpenFullMutex     -- Ok for sqlite3_open2()
                 | SQLOpenSharedCache   -- Ok for sqlite3_open2()
                 | SQLOpenPrivateCache  -- Ok for sqlite3_open2()
                 | SQLOpenWAL           -- VFS only
                 deriving (Eq, Show)

foreign import ccall "sqlite3_open_v2"
    c_sqlite3_open_v2 :: CString
                      -> Ptr (Ptr CDatabase)
                      -> CInt
                      -> CString
                      -> IO CError

-- Result is not exported, this is a copy from Database.SQLite3.Direct
type Result a = Either Error a

-- toResult is not exported, this is a copy from Database.SQLite3.Direct
toResult :: a -> CError -> Result a
toResult a (CError 0) = Right a
toResult _ code       = Left $ decodeError code

-- toUtf8 is not exported, this is a copy from Database.SQLite3
toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . encodeUtf8

-- appendShow is not exported, this is a copy from Database.SQLite3
appendShow :: Show a => Text -> a -> Text
appendShow txt a = txt `T.append` (T.pack . show) a

-- checkErrorMsg is not exported, this is a copy from Database.SQLite3
checkErrorMsg :: Text -> Either (Error, Utf8) a -> IO a
checkErrorMsg fn result = case result of
    Left (err, msg) -> throwSQLError (DetailMessage msg) fn err
    Right a         -> return a

-- throwSQLError is not exported, this is a copy from Database.SQLite3
throwSQLError :: DetailSource -> Text -> Error -> IO a
throwSQLError detailSource context sqlerror = do
    Utf8 details <- renderDetailSource detailSource
    throwIO SQLError
        { sqlError        = sqlerror
        , sqlErrorDetails = decodeUtf8With lenientDecode details
        , sqlErrorContext = context
        }

-- DetailSource is not exported, this is a copy from Database.SQLite3
data DetailSource
    = DetailDatabase    Database
    | DetailStatement   Statement
    | DetailMessage     Utf8

-- renderDetailSource is not exported, this is a copy from Database.SQLite3
renderDetailSource :: DetailSource -> IO Utf8
renderDetailSource src = case src of
    DetailDatabase db ->
        Direct.errmsg db
    DetailStatement stmt -> do
        db <- Direct.getStatementDatabase stmt
        Direct.errmsg db
    DetailMessage msg ->
        return msg

_open2 :: Utf8 -> Int -> Maybe Utf8 -> IO (Either (Error, Utf8) Database)
_open2 (Utf8 path) flags mzvfs =
    BS.useAsCString path $ \path' ->
    useAsMaybeCString mzvfs $ \zvfs' ->
    alloca $ \database -> do
        rc <- c_sqlite3_open_v2 path' database (toEnum flags) zvfs'
        db <- Database <$> peek database
            -- sqlite3_open2 returns a sqlite3 even on failure.
            -- That's where we get a more descriptive error message.
        case toResult () rc of
            Left err -> do
                msg <- errmsg db -- This returns "out of memory" if db is null.
                _   <- close db  -- This is harmless if db is null.
                return $ Left (err, msg)
            Right () ->
                if db == Database nullPtr
                    then fail "sqlite3_open unexpectedly returned NULL"
                    else return $ Right db

    where useAsMaybeCString :: Maybe Utf8 -> (CString -> IO a) -> IO a
          useAsMaybeCString (Just (Utf8 zvfs)) f = BS.useAsCString zvfs f
          useAsMaybeCString _ f = f nullPtr

open2 :: Text -> [SQLOpenFlag] -> SQLVFS -> IO Database
open2 path flags zvfs =
    _open2 (toUtf8 path) (makeFlag flags) (toUtf8 <$> toMaybeText zvfs)
        >>= checkErrorMsg ("open " `appendShow` path)
    where
        makeFlag = foldr (.|.) 0 . fmap toNum

        toNum SQLOpenReadOnly      = 0x00000001
        toNum SQLOpenReadWrite     = 0x00000002
        toNum SQLOpenCreate        = 0x00000004
        toNum SQLOpenDeleteOnClose = 0x00000008
        toNum SQLOpenExclusive     = 0x00000010
        toNum SQLOpenAutoProxy     = 0x00000020
        toNum SQLOpenURI           = 0x00000040
        toNum SQLOpenMemory        = 0x00000080
        toNum SQLOpenMainDB        = 0x00000100
        toNum SQLOpenTempDB        = 0x00000200
        toNum SQLOpenTransientDB   = 0x00000400
        toNum SQLOpenMainJournal   = 0x00000800
        toNum SQLOpenTempJournal   = 0x00001000
        toNum SQLOpenSubJournal    = 0x00002000
        toNum SQLOpenMasterJournal = 0x00004000
        toNum SQLOpenNoMutex       = 0x00008000
        toNum SQLOpenFullMutex     = 0x00010000
        toNum SQLOpenSharedCache   = 0x00020000
        toNum SQLOpenPrivateCache  = 0x00040000
        toNum SQLOpenWAL           = 0x00080000
