-- Migration: create_cities
-- Created at: 2019-04-08 07:41:52
-- ====  UP  ====

BEGIN;

CREATE TABLE cities (
  id          SERIAL PRIMARY KEY,
  name        VARCHAR(200) NOT NULL,
  asciiname   VARCHAR(200) NOT NULL,
  country     CHAR(2),
  latitude    FLOAT NOT NULL,
  longitude   FLOAT NOT NULL,
  location    GEOGRAPHY(POINT) NOT NULL
);

CREATE INDEX cities_location ON cities USING GIST(location);

COMMIT;

-- ==== DOWN ====

BEGIN;

DROP TABLE cities;

COMMIT;
