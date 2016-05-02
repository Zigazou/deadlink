#!/bin/bash

function assert {
  local message="$1"
  shift
  "$@"
  local rc=$?
  [ $rc -eq 0 ] && return 0
  set $(caller)
  local date=$(date "+%Y-%m-%d %T%z")
  echo "$date $2 [$$]: $message (line=$1, rc=$rc)" >&2
  exit $rc
}

assert "sqlite3 not found" which sqlite3 > /dev/null
assert "deadlink.db has already been created" test ! -f deadlink.db

sqlite3 deadlink.db <<EOF
    CREATE TABLE link ( url TEXT PRIMARY KEY
                      , httpcode INTEGER
                      , contenttype TEXT
                      , checkdate TEXT
                      , parsedate TEXT
                      );

    CREATE TABLE parent ( parenturl TEXT
                        , childurl TEXT
                        , PRIMARY KEY (parenturl, childurl)
                        );
EOF