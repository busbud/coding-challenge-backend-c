DROP MATERIALIZED VIEW IF EXISTS cities_mv;
DROP INDEX IF EXISTS ascii_name_idx;

-- Create materialized view of refined cols and fixed minimum population requirement 
CREATE MATERIALIZED VIEW cities_mv AS
SELECT id, name, ascii, lat, long, country, admin1, population
FROM cities
WHERE population > 5000;

-- Create index for partial searches on name
CREATE INDEX ascii_name_idx
ON cities_mv
USING GIN(to_tsvector('english', ascii));

REFRESH MATERIALIZED VIEW cities_mv WITH DATA;