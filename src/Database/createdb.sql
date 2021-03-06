-- Database creation script for Deadlink
--
-- This file is included into the deadlink executable when the Deadlink
-- project is being built. It allows the deadlink executable to create
-- databases without requiring an external script.
--
-- This script is meant to create an SQLite3 database.

CREATE TABLE link ( url TEXT PRIMARY KEY
                  , httpcode INTEGER
                  , curlcode INTEGER
                  , contenttype TEXT
                  , checkdate TEXT
                  , parsedate TEXT
                  );

CREATE TABLE parent ( parenturl TEXT
                    , childurl TEXT
                    , iteration INTEGER
                    , PRIMARY KEY (parenturl, childurl)
                    );

CREATE TABLE http ( code INTEGER
                  , description TEXT
                  );

INSERT INTO http (code, description)
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

CREATE TABLE curl ( code INTEGER
                  , description TEXT
                  );

INSERT INTO curl (code, description)
VALUES (0, 'OK')
     , (1, 'Unsupported protocol')
     , (2, 'Failed init')
     , (3, 'URL malformat')
     , (4, 'URL malformat user')
     , (5, 'Could not resolve proxy')
     , (6, 'Could not resolve host')
     , (7, 'Could not connect')
     , (8, 'FTP weird server reply')
     , (9, 'FTP access denied')
     , (10, 'FTP user password incorrect')
     , (11, 'FTP weird PASS Reply')
     , (12, 'FTP weird USER Reply')
     , (13, 'FTP weird PASV reply')
     , (14, 'FTP weird 227 format')
     , (15, 'FTP cannot get host')
     , (16, 'FTP cannot reconnect')
     , (17, 'FTP could not set binary')
     , (18, 'Partial file')
     , (19, 'FTP couldnt retrieve file')
     , (20, 'FTP write error')
     , (21, 'FTP quote error')
     , (22, 'HTTP returned error')
     , (23, 'Write error')
     , (24, 'Malformat error')
     , (25, 'FTP could not store file')
     , (26, 'Read error')
     , (27, 'Out of memory')
     , (28, 'Operation timeout')
     , (29, 'FTP could not set ASCII')
     , (30, 'FTP port failed')
     , (31, 'FTP could not use REST')
     , (32, 'FTP could not get size')
     , (33, 'HTTP range error')
     , (34, 'HTTP post error')
     , (35, 'SSL connect error')
     , (36, 'Bad download resume')
     , (37, 'File could not read file')
     , (38, 'LDAP cannot bind')
     , (39, 'LDAP search failed')
     , (40, 'Library not found')
     , (41, 'Function not found')
     , (42, 'Aborted by callback')
     , (43, 'Bad function argument')
     , (44, 'Bad calling order')
     , (45, 'Interface failed')
     , (46, 'Bad password entered')
     , (47, 'Too many redirects')
     , (48, 'Unknown telnet option')
     , (49, 'Telnet option syntax')
     , (50, 'Obsolete')
     , (51, 'SSL peer certificate')
     , (52, 'Got nothing')
     , (53, 'SSL engine not found')
     , (54, 'SSL engine set failed')
     , (55, 'SEND error')
     , (56, 'RECV error')
     , (57, 'Share in use')
     , (58, 'SSL cert problem')
     , (59, 'SSL cipher')
     , (60, 'SSL CA cert')
     , (61, 'Bad content encoding')
     , (62, 'LDAP invalid URL')
     , (63, 'Filesize exceeded')
     , (64, 'FTP SSL failed')
     , (65, 'Send fail rewind')
     , (66, 'SSL engine init failed')
     , (67, 'Login denied')
     , (68, 'TFTP not found')
     , (69, 'TFTP perm')
     , (70, 'TFTP disk full')
     , (71, 'TFTP illegal')
     , (72, 'TFTP unknown id')
     , (73, 'TFTP exists')
     , (74, 'TFTP no such user')
     , (75, 'Conv failed')
     , (76, 'Conv required')
     , (77, 'SSL CA cert bad file')
     , (78, 'Remove file not found')
     , (79, 'SSH')
     , (80, 'SSL shutdown failed')
     , (81, 'Again')
     , (82, 'SSL CRL bad file')
     , (83, 'SSL issuer error');
