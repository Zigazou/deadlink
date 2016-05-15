SELECT COUNT(*)
FROM   link
WHERE  checkdate IS NOT NULL
OR     parsedate IS NOT NULL;
