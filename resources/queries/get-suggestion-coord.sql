-- $1 -> searchTearm
-- $2 -> long
-- $3 -> lat
select
       name,
       lat as latitude,
       long as longitude,
       (0.3 * (1 - (levenshtein($1, ascii)::numeric / max(levenshtein($1, ascii)::numeric) over ()))) +
       (0.7 * (1 - ((coordinates <-> ST_SetSRID( ST_Point($2, $3), 4326)) / max( coordinates <-> ST_SetSRID( ST_Point($2, $3), 4326)) over()))) as score
from suggestions_service.cities
where ascii ilike $1 || '%'
and population > 5000
order by
       (0.3 * (1 - (levenshtein($1, ascii)::numeric / max(levenshtein($1, ascii)::numeric) over ()))) +
       (0.7 * (1 - ((coordinates <-> ST_SetSRID( ST_Point($2, $3), 4326)) / max( coordinates <-> ST_SetSRID( ST_Point($2, $3), 4326)) over()))) desc
limit 10;