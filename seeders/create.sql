CREATE TABLE geoname ( geonameid int, name varchar(200), asciiname varchar(200), alternatenames text, latitude float, longitude float, fclass char(1), fcode varchar(10), country varchar(2), cc2 varchar(60), admin1 varchar(20), admin2 varchar(80), admin3 varchar(20), admin4 varchar(20), population bigint, elevation int, gtopo30 int, timezone varchar(40), moddate date );

ALTER TABLE ONLY geoname ADD CONSTRAINT pk_geonameid PRIMARY KEY (geonameid);
CREATE INDEX IF NOT EXISTS LAT_LON_IDX ON geoname (latitude, longitude);
CREATE INDEX IF NOT EXISTS GEONAME_IDX ON geoname (name);
CREATE INDEX IF NOT EXISTS NAME_LAT_LON_IDX ON geoname (name, latitude, longitude);
CREATE INDEX ON geoname USING gist (POINT(latitude, longitude));

CREATE EXTENSION unaccent;
CREATE EXTENSION postgis;

\copy geoname (geonameid,name,asciiname,alternatenames,latitude,longitude,fclass,fcode,country,cc2, admin1,admin2,admin3,admin4,population,elevation,gtopo30,timezone,moddate) from '../data/cities_canada-usa.tsv' with CSV HEADER QUOTE E'\b'  DELIMITER E'\t' null as '';

