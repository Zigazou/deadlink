#!/bin/bash

function execdb() {
    echo "$1" | sqlite3 deadlink.db
}

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

count404link=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  httpcode = 404;
")

count403link=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  httpcode = 403;
")

count301link=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  httpcode = 301;
")

count302link=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  httpcode = 302;
")

countcheckedlink=$(execdb "
    SELECT COUNT(*)
    FROM   link
    WHERE  checkdate IS NOT NULL
    OR     parsedate IS NOT NULL;
")

mostuseddeadlink=$(execdb "
    SELECT   childurl
    FROM     parent, link
    WHERE    url = childurl
    AND      httpcode = 404
    GROUP BY childurl
    HAVING   COUNT(parenturl) > 30;
")

otherhttpcode=$(execdb "
    SELECT   httpcode, COUNT(*)
    FROM     link
    WHERE    httpcode NOT IN (404, 403, 301, 302)
    GROUP BY httpcode
    ORDER BY httpcode ASC;
")

printf -- '\nStatistics for %s\n\n' "$baselink"
printf -- '- Links: %d\n' "$countlink"
printf -- '- HTML pages: %d\n' "$counthtmlpage"
printf -- '- 404 links: %d\n' "$count404link"
printf -- '- 403 links: %d\n' "$count403link"
printf -- '- 301 links: %d\n' "$count301link"
printf -- '- 302 links: %d\n' "$count302link"
printf -- '- Checked links: %d\n' "$countcheckedlink"
printf -- '- Most used dead links:\n%s\n' "$mostuseddeadlink"
printf -- '- Other HTTP codes:\n%s\n' "$otherhttpcode"

printf -- '\n'
