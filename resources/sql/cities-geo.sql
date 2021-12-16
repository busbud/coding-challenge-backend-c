select name,
       admin1,
       country,
       lat,
       long,
       (0.7 * (1 - ((ST_Distance(ST_MakePoint($2, $3)::geography, ST_MakePoint(long, lat)::geography)) / 1000) /
                   max(ST_Distance(ST_MakePoint($2, $3)::geography, ST_MakePoint(long, lat)::geography)) / 1000)) +
       ((0.3 * (1 - (levenshtein($1, name)::numeric / max(levenshtein($1, name)::numeric) over ())))) as score
from city
where LOWER(name) like LOWER($1) || '%'
group by name, admin1, country, lat, long, localisation
order by score desc limit 10;
