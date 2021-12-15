select name,
       admin1,
       country,
       lat,
       long,
       (max(ST_SetSRID(ST_Point($2, $3), 4326)) over ())                                    as score
from city
where name like $1 || '%'
order by score desc limit 20;
