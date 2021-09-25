drop table if exists public.cities;

CREATE TABLE public.cities (
	id bigint NOT NULL,
	"name" varchar(200) NOT NULL,
	ascii varchar(200) NULL,
	alt_name text NULL,
	lat float8 NULL,
	long float8 NULL,
	feat_class bpchar(1) NULL,
	feat_code varchar(10) NULL,
	country varchar(2) NULL,
	cc2 varchar(60) NULL,
	admin1 varchar(20) NULL,
	admin2 varchar(80) NULL,
	admin3 varchar(20) NULL,
	admin4 varchar(20) NULL,
	population bigint NULL,
	elevation int4 NULL,
	dem int4 NULL,
	tz varchar(40) NULL,
	modified_at varchar(10) NULL,
	CONSTRAINT cities_pkey PRIMARY KEY (id)
);

create EXTENSION if not exists  pg_trgm;

CREATE OR REPLACE FUNCTION calculate_distance(lat1 float, lon1 float, lat2 float, lon2 float)
RETURNS float AS $dist$
    DECLARE
        dist float = 0;
        radlat1 float;
        radlat2 float;
        theta float;
        radtheta float;
    BEGIN
        IF lat1 = lat2 AND lon1 = lon2
            THEN RETURN dist;
        ELSE
            radlat1 = pi() * lat1 / 180;
            radlat2 = pi() * lat2 / 180;
            theta = lon1 - lon2;
            radtheta = pi() * theta / 180;
            dist = sin(radlat1) * sin(radlat2) + cos(radlat1) * cos(radlat2) * cos(radtheta);

            IF dist > 1 THEN dist = 1; END IF;

            dist = acos(dist);
            dist = dist * 180 / pi();
            dist = dist * 60 * 1.1515 * 1.609344;

            RETURN dist;
        END IF;
    END;
$dist$ LANGUAGE plpgsql;

COPY cities 
FROM '/mnt/db/cities_canada-usa.csv'
DELIMITER E','
CSV HEADER;
