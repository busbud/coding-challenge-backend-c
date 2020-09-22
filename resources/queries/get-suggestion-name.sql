-- $1 searchTerm
select
       name,
       lat as latitude,
       long as longitude,
       1 - (levenshtein($1, ascii)::numeric / max(levenshtein($1, ascii)::numeric) over ()) as score
from suggestions_service.cities
where ascii ilike $1 || '%'
and population > 5000
order by
      1 - (levenshtein($1, ascii)::numeric / max(levenshtein($1, ascii)::numeric) over ()) desc
limit 10;