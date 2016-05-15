{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : MissingSQLite3
Description : Defines the open_v2 SQLite3 missing function
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

The package direct-sqlite3 does not define the open_v2 SQLite3 function. This
function allows, among other things, to set the flags defining how the open
function must behave. For example, it allows you to open an existing database
and not creating it if it doesn’t exist as is the case for the open function.

MissingSQLite3 defines the open_v2 SQLite3 function.

Since it is not part of the original package, some functions were copied from
direct-sqlite3 into MissingSQLite3 because these functions were not exported.
-}
module Database.MissingSQLite3
( open_v2
, SQLiteFlag (..)
, SQLiteVFS (..)
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

data SQLiteVFS = SQLiteVFSDefault
               | SQLiteVFSUnix
               | SQLiteVFSUnixDotFile
               | SQLiteVFSUnixExcl
               | SQLiteVFSUnixNone
               | SQLiteVFSUnixNamedSem
               deriving (Eq, Show)

toMaybeText :: SQLiteVFS -> Maybe Text
toMaybeText SQLiteVFSDefault = Nothing
toMaybeText SQLiteVFSUnix = Just "unix"
toMaybeText SQLiteVFSUnixDotFile = Just "unix-dotfile"
toMaybeText SQLiteVFSUnixExcl = Just "unix-excl"
toMaybeText SQLiteVFSUnixNone = Just "unix-none"
toMaybeText SQLiteVFSUnixNamedSem = Just "unix-namedsem"

data SQLiteFlag = SQLiteOpenReadOnly      -- Ok for sqlite3_open_v2()
                | SQLiteOpenReadWrite     -- Ok for sqlite3_open_v2()
                | SQLiteOpenCreate        -- Ok for sqlite3_open_v2()
                | SQLiteOpenDeleteOnClose -- VFS only
                | SQLiteOpenExclusive     -- VFS only
                | SQLiteOpenAutoProxy     -- VFS only
                | SQLiteOpenURI           -- Ok for sqlite3_open_v2()
                | SQLiteOpenMemory        -- Ok for sqlite3_open_v2()
                | SQLiteOpenMainDB        -- VFS only
                | SQLiteOpenTempDB        -- VFS only
                | SQLiteOpenTransientDB   -- VFS only
                | SQLiteOpenMainJournal   -- VFS only
                | SQLiteOpenTempJournal   -- VFS only
                | SQLiteOpenSubJournal    -- VFS only
                | SQLiteOpenMasterJournal -- VFS only
                | SQLiteOpenNoMutex       -- Ok for sqlite3_open_v2()
                | SQLiteOpenFullMutex     -- Ok for sqlite3_open_v2()
                | SQLiteOpenSharedCache   -- Ok for sqlite3_open_v2()
                | SQLiteOpenPrivateCache  -- Ok for sqlite3_open_v2()
                | SQLiteOpenWAL           -- VFS only
                deriving (Eq, Show)

instance Enum SQLiteFlag where
    fromEnum SQLiteOpenReadOnly      = 0x00000001
    fromEnum SQLiteOpenReadWrite     = 0x00000002
    fromEnum SQLiteOpenCreate        = 0x00000004
    fromEnum SQLiteOpenDeleteOnClose = 0x00000008
    fromEnum SQLiteOpenExclusive     = 0x00000010
    fromEnum SQLiteOpenAutoProxy     = 0x00000020
    fromEnum SQLiteOpenURI           = 0x00000040
    fromEnum SQLiteOpenMemory        = 0x00000080
    fromEnum SQLiteOpenMainDB        = 0x00000100
    fromEnum SQLiteOpenTempDB        = 0x00000200
    fromEnum SQLiteOpenTransientDB   = 0x00000400
    fromEnum SQLiteOpenMainJournal   = 0x00000800
    fromEnum SQLiteOpenTempJournal   = 0x00001000
    fromEnum SQLiteOpenSubJournal    = 0x00002000
    fromEnum SQLiteOpenMasterJournal = 0x00004000
    fromEnum SQLiteOpenNoMutex       = 0x00008000
    fromEnum SQLiteOpenFullMutex     = 0x00010000
    fromEnum SQLiteOpenSharedCache   = 0x00020000
    fromEnum SQLiteOpenPrivateCache  = 0x00040000
    fromEnum SQLiteOpenWAL           = 0x00080000

    toEnum 0x00000001 = SQLiteOpenReadOnly
    toEnum 0x00000002 = SQLiteOpenReadWrite
    toEnum 0x00000004 = SQLiteOpenCreate
    toEnum 0x00000008 = SQLiteOpenDeleteOnClose
    toEnum 0x00000010 = SQLiteOpenExclusive
    toEnum 0x00000020 = SQLiteOpenAutoProxy
    toEnum 0x00000040 = SQLiteOpenURI
    toEnum 0x00000080 = SQLiteOpenMemory
    toEnum 0x00000100 = SQLiteOpenMainDB
    toEnum 0x00000200 = SQLiteOpenTempDB
    toEnum 0x00000400 = SQLiteOpenTransientDB
    toEnum 0x00000800 = SQLiteOpenMainJournal
    toEnum 0x00001000 = SQLiteOpenTempJournal
    toEnum 0x00002000 = SQLiteOpenSubJournal
    toEnum 0x00004000 = SQLiteOpenMasterJournal
    toEnum 0x00008000 = SQLiteOpenNoMutex
    toEnum 0x00010000 = SQLiteOpenFullMutex
    toEnum 0x00020000 = SQLiteOpenSharedCache
    toEnum 0x00040000 = SQLiteOpenPrivateCache
    toEnum 0x00080000 = SQLiteOpenWAL
    toEnum _ = undefined

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

_open_v2 :: Utf8 -> Int -> Maybe Utf8 -> IO (Either (Error, Utf8) Database)
_open_v2 (Utf8 path) flags mzvfs =
    BS.useAsCString path $ \path' ->
    useAsMaybeCString mzvfs $ \zvfs' ->
    alloca $ \database -> do
        rc <- c_sqlite3_open_v2 path' database (toEnum flags) zvfs'
        db <- Database <$> peek database
            -- sqlite3_open_v2 returns a sqlite3 even on failure.
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

open_v2 :: Text -> SQLiteFlag -> SQLiteVFS -> IO Database
open_v2 path flags zvfs =
    _open_v2 (toUtf8 path) (fromEnum flags) (toUtf8 <$> toMaybeText zvfs)
        >>= checkErrorMsg ("open " `appendShow` path)