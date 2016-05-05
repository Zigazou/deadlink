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

baselink=$(execdb "
    SELECT childurl
    FROM   parent
    WHERE  parenturl = ''
    OR     parenturl IS NULL;
")

countlink=$(execdb "
    SELECT COUNT(*)
    FROM link;
")

counthtmlpage=$(execdb "
    SELECT   COUNT(*)
    FROM     link
    WHERE    contenttype LIKE 'text/html%';
")

countcheckedlink=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  checkdate IS NOT NULL
    OR     parsedate IS NOT NULL;
")

countexternallink=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  url NOT LIKE '$baselink%';
")

mostuseddeadlink=$(execdb "
    SELECT   childurl
    FROM     parent, link
    WHERE    url = childurl
    AND      httpcode = 404
    GROUP BY childurl
    HAVING   COUNT(parenturl) > 30;
")

httpcode=$(execdb "
    SELECT   '  - '
          || code.httpcode
          || ' '
          || code.description
          || ': '
          || COUNT(*)
    FROM     link, code
    WHERE    code.httpcode = link.httpcode
    GROUP BY link.httpcode
    ORDER BY link.httpcode ASC;
")

linktype=$(execdb "
    SELECT   '  - '
          || CASE WHEN contenttype = '' THEN '[unknown]' ELSE contenttype END
          || ': '
          || COUNT(*)
    FROM     link
    GROUP BY contenttype;
")

printf -- '\nStatistics for %s\n\n' "$baselink"
printf -- '- Links: %d\n' "$countlink"
printf -- '- External links: %d\n' "$countexternallink"
printf -- '- HTML pages: %d\n' "$counthtmlpage"
printf -- '- Checked links: %d\n' "$countcheckedlink"
printf -- '- Most used dead links:\n%s\n' "$mostuseddeadlink"
printf -- '- HTTP codes:\n%s\n' "$httpcode"
printf -- '- Content types:\n%s\n' "$linktype"

printf -- '\n'
