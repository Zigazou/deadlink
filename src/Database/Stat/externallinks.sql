SELECT COUNT(*)
FROM   link
WHERE  url NOT LIKE ( SELECT childurl || '%'
                      FROM   parent
                      WHERE  parenturl = ''
                      OR     parenturl IS NULL
                    );
