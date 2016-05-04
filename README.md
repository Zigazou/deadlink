DeadLink
========

DeadLink is a dead link checker written in Haskell.

Building deadlink
-----------------

DeadLink has been written in Haskell and is compiled with GHC 7.10. It is
meant to be run under Linux.

It requires Curl and SQLite3 development libraries.

To build `deadlink`:

    cabal build

The `deadlink` executable will have been created in `./dist/build/deadlink`.

Usage
-----

First `deadlink` needs an SQLite3 database named `deadlink.db`.

To create `deadlink.db`:

    db/createdb.bash

This script creates an empty SQLite3 database structured for `deadlink`. The
use of a SQLite3 database has benefits:

- `deadlink` can be stopped and resumed at any time without losing links,
- the database can be queried with standard SQL statements,
- the database consists of one file which can be copied or saved for later use.

Once the database has been created, you can run `deadlink`:

    deadlink <base URL>

For example:

    deadlink http://www.example.com/

Everytime `deadlink` needs to be resumed, it requires the same base URL to
be specified.

At the moment, `deadlink` needs to be restarted by hand after each iteration.

Querying the database
---------------------

The `deadlink` database is a SQLite3 database. Querying it is as simple as
running:

    sqlite3 deadlink.db

Query examples:

    -- Get number of links in the database
    SELECT COUNT(*)
    FROM   link;

    -- Get number of links that have been checked or parsed
    SELECT COUNT(*)
    FROM   link
    WHERE  checkdate IS NOT NULL
    OR     parsedate IS NOT NULL;

    -- Get number of links which returned a 404 HTTP code
    SELECT COUNT(*)
    FROM   link
    WHERE  httpcode = 404;

    -- Get dead links and the pages on which they have been found
    SELECT url, parenturl
    FROM   link, parent
    WHERE  httpcode = 404
    AND    childurl = url;

Notes
-----

Writes to the database are done after 50 links checked/parsed. If `deadlink`
is stopped, there is potentially a maximum of 50 links that will need to be
checked again.

`deadlink` will check every links found on HTML pages but only HTML pages
belonging to the base URL will be parsed.

Everytime `deadlink` needs to be resumed, it requires the same base URL to
be specified.