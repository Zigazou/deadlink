SELECT   CASE WHEN contenttype = ''    THEN '[empty]'
              WHEN contenttype IS NULL THEN '[not specified]'
              ELSE contenttype
              END,
         COUNT(*)
FROM     link
GROUP BY contenttype
ORDER BY COUNT(contenttype) DESC;
