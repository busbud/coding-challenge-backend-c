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

/*
create EXTENSION if not exists  pg_trgm;

CREATE extension  if not exists fuzzystrmatch;

select name, 1 / (1 + cast(levenshtein(name,'Abbotsford') as float))
FROM cities
order by levenshtein(name,'Abbotsford') asc
limit 10;

select name, SIMILARITY(name,'Abot')
FROM cities
order by SIMILARITY(name,'Abbotsfo') desc
limit 10;
 * */