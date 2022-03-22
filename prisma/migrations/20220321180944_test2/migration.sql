--Create trigram index using GIN

CREATE INDEX CONCURRENTLY index_cities_trigram
ON cities
USING GIN(cities_indexed_name);
