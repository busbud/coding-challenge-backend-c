BEGIN;

CREATE TABLE cities_tmp (
  name        VARCHAR(200) NOT NULL,
  asciiname   VARCHAR(200) NOT NULL,
  latitude    FLOAT NOT NULL,
  longitude   FLOAT NOT NULL,
  country     CHAR(2)
);

COPY cities_tmp(name, asciiname, latitude, longitude, country)
  FROM '/app/data/cities_canada-usa.tsv.tmp'
  DELIMITER E'\t'
  CSV HEADER;

INSERT INTO cities (name, asciiname, country, latitude, longitude, location)
  SELECT name, asciiname, country, latitude, longitude, ST_POINT(longitude, latitude)
  FROM cities_tmp;

DROP TABLE cities_tmp;

COMMIT;
