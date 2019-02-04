SELECT 
	state,
	COUNT(*) AS n,
	AVG(CASE WHEN sex = "female" THEN 1 ELSE 0 END) AS prop_female,
	AVG(CASE WHEN birthyr <= 1945 THEN 1 ELSE 0 END) AS prop_silent,
	AVG(CASE WHEN birthyr >= 1946 AND birthyr <= 1964 THEN 1 ELSE 0 END) AS prop_boomer,
	AVG(CASE WHEN birthyr >= 1965 AND birthyr <= 1980 THEN 1 ELSE 0 END) AS prop_genx,
	AVG(CASE WHEN birthyr >= 1981 AND birthyr <= 1996 THEN 1 ELSE 0 END) AS prop_millenial
FROM voterfile -- whatever the name of the voter file is
WHERE e2016GVM != "" -- only care about electorate
GROUP BY state;
