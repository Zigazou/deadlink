SELECT   code.httpcode,
         code.description,
         COUNT(*)
FROM     link, code
WHERE    code.httpcode = link.httpcode
GROUP BY link.httpcode
ORDER BY link.httpcode ASC;
