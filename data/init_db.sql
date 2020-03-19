CREATE TABLE public.cities(
  id BIGINT NOT NULL PRIMARY KEY,
  name VARCHAR(200) NOT NULL,
  ascii VARCHAR(200) NOT NULL,
  alt_name TEXT,
  lat NUMERIC(10, 5),
  long NUMERIC(10, 5),
  feat_class VARCHAR(1),
  feat_code VARCHAR(10),
  country VARCHAR(2) NOT NULL,
  cc2 VARCHAR(60),
  admin1 VARCHAR(20) NOT NULL,
  admin2 VARCHAR(20),
  admin3 VARCHAR(20),
  admin4 VARCHAR(20),
  population INTEGER NOT NULL,
  elevation INTEGER,
  dem INTEGER,
  tz VARCHAR(40) NOT NULL,
  modified_at DATE NOT NULL
);

COPY public.cities FROM '/data/cities_canada-usa.tsv' CSV HEADER DELIMITER E'\t' QUOTE '&';