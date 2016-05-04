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

    CREATE TABLE code ( httpcode INTEGER
                      , description TEXT
                      );

    INSERT INTO code (httpcode, description)
    VALUES (0, 'CURL error')
         , (100, 'Continue')
         , (101, 'Switching Protocols')
         , (102, 'Processing')
         , (200, 'OK')
         , (201, 'Created')
         , (202, 'Accepted')
         , (203, 'Non-Authoritative Information')
         , (204, 'No Content')
         , (205, 'Reset Content')
         , (206, 'Partial Content')
         , (207, 'Multi-Status')
         , (210, 'Content Different')
         , (226, 'IM Used')
         , (300, 'Multiple Choices')
         , (301, 'Moved Permanently')
         , (302, 'Moved Temporarily')
         , (303, 'See Other')
         , (304, 'Not Modified')
         , (305, 'Use Proxy')
         , (306, '(Reserved)')
         , (307, 'Temporary Redirect')
         , (308, 'Permanent Redirect')
         , (310, 'Too many Redirects')
         , (400, 'Bad Request')
         , (401, 'Unauthorized')
         , (402, 'Payment Required')
         , (403, 'Forbidden')
         , (404, 'Not Found')
         , (405, 'Method Not Allowed')
         , (406, 'Not Acceptable')
         , (407, 'Proxy Authentication Required')
         , (408, 'Request Time-out')
         , (409, 'Conflict')
         , (410, 'Gone')
         , (411, 'Length Required')
         , (412, 'Precondition Failed')
         , (413, 'Request Entity Too Large')
         , (414, 'Request-URI Too Long')
         , (415, 'Unsupported Media Type')
         , (416, 'Requested range unsatisfiable')
         , (417, 'Expectation failed')
         , (418, 'I’m a teapot')
         , (421, 'Bad mapping / Misdirected Request')
         , (422, 'Unprocessable entity')
         , (423, 'Locked')
         , (424, 'Method failure')
         , (425, 'Unordered Collection')
         , (426, 'Upgrade Required')
         , (428, 'Precondition Required')
         , (429, 'Too Many Requests')
         , (431, 'Request Header Fields Too Large')
         , (449, 'Retry With')
         , (450, 'Blocked by Windows Parental Controls')
         , (451, 'Unavailable For Legal Reasons')
         , (456, 'Unrecoverable Error')
         , (499, 'client has closed connection')
         , (500, 'Internal Server Error')
         , (501, 'Not Implemented')
         , (502, 'Bad Gateway ou Proxy Error')
         , (503, 'Service Unavailable')
         , (504, 'Gateway Time-out')
         , (505, 'HTTP Version not supported')
         , (506, 'Variant also negociate')
         , (507, 'Insufficient storage')
         , (508, 'Loop detected')
         , (509, 'Bandwidth Limit Exceeded')
         , (510, 'Not extended')
         , (511, 'Network authentication required')
         , (520, 'Web server is returning an unknown error');

EOF
