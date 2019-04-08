BEGIN;

CREATE TABLE cities_tmp (
  name        VARCHAR(200) NOT NULL,
  latitude    FLOAT NOT NULL,
  longitude   FLOAT NOT NULL,
  country     CHAR(2)
);

COPY cities_tmp(name, latitude, longitude, country)
  FROM '/app/data/cities_canada-usa.tsv.tmp'
  DELIMITER E'\t'
  CSV HEADER;

INSERT INTO cities (name, country, latitude, longitude, location)
  SELECT name, country, latitude, longitude, ST_POINT(longitude, latitude)
  FROM cities_tmp;

DROP TABLE cities_tmp;

COMMIT;
