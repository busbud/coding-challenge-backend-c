-- $1 searchTerm
select
       name,
       country,
       admin1,
       lat,
       long,
       1 - (levenshtein($1, name)::numeric / max(levenshtein($1, name)::numeric) over ()) as score
from suggestions_service.cities
where name ilike $1 || '%'
order by
      1 - (levenshtein($1, name)::numeric / max(levenshtein($1, name)::numeric) over ()) desc
limit 10;