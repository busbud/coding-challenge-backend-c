select name,
       admin1,
       country,
       lat,
       long,
       (0.2 * (1 - (levenshtein($1, name)::numeric / max(levenshtein($1, name)::numeric) over ()))) +
       (0.8 * (1 - ((localisation <-> ST_SetSRID(ST_Point($2, $3), 4326)) /
                    max(localisation <-> ST_SetSRID(ST_Point($2, $3), 4326)) over()))) as score
from city
where LOWER (name) like LOWER ($1) || '%'
order by score desc limit 20;
