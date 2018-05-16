#!/bin/bash

function execdb() {
    echo "$1" | sqlite3 "$DATABASE"
}

if [ "$1" = "" ]
then
    DATABASE="deadlink.db"
else
    DATABASE="$1"
fi

if [ ! -f "$DATABASE" ]
then
    printf -- "Database '%s' not found!\n" "$DATABASE"
    exit 1
fi

req="
    SELECT   childurl || ' ('
                      || http.description
                      || ', '
                      || curl.description
                      || ')',
             GROUP_CONCAT(parenturl, '|')
    FROM     parent, link, http, curl
    WHERE    url = childurl
    AND      http.code = httpcode
    AND      curl.code = curlcode
    AND      httpcode IN (0, 404)
    GROUP BY childurl;
"

execdb "$req" \
    | sed 's/|/|    /g' \
    | tr '|' '\n'

