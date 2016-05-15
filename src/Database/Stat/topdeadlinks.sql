SELECT   childurl
FROM     parent, link
WHERE    url = childurl
AND      httpcode = 404
GROUP BY childurl
ORDER BY COUNT(parenturl) DESC
LIMIT    10;
