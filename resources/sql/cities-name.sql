select name,
       admin1,
       country,
       lat,
       long,
       1 - (levenshtein($1, name)::numeric / max(levenshtein($1, name)::numeric) over ()) as score
from city
where name like $1 || '%'
order by score desc limit 20;
