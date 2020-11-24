COPY cities
FROM '/data/cities_canada-usa.tsv'
WITH (
    FORMAT CSV,
    DELIMITER E'\t',
    HEADER
);