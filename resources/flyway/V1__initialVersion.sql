CREATE
EXTENSION IF NOT EXISTS fuzzystrmatch;
CREATE
EXTENSION IF NOT EXISTS postgis;
CREATE
EXTENSION IF NOT EXISTS btree_gist;

CREATE TABLE suggestions.city
(
    id           INTEGER PRIMARY KEY,
    name         VARCHAR(100),
    ascii        VARCHAR(100),
    alt_name     VARCHAR(100),
    lat          NUMERIC,
    long         NUMERIC,
    feat_class   VARCHAR(20),
    feat_code    VARCHAR(20),
    country      VARCHAR(20),
    cc2          VARCHAR(60),
    admin1       VARCHAR(60),
    admin2       VARCHAR(60),
    admin3       VARCHAR(60),
    admin4       VARCHAR(60),
    population   INTEGER,
    elevation    INTEGER,
    dem          INTEGER,
    tz           VARCHAR(60),
    modified_at  TIMESTAMP WITH TIME ZONE,
    localisation GEOGRAPHY
)
