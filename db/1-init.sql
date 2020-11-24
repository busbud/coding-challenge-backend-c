DROP TABLE IF EXISTS "cities";

CREATE TABLE "cities" (
    "id" INTEGER PRIMARY KEY,
    "name" VARCHAR(200),
    "ascii" VARCHAR(200),
    "alt_name" VARCHAR(10000),
    "lat" NUMERIC(8,6),
    "long" NUMERIC(9,6),
    "feature_class" CHAR(1),
    "feature_code" VARCHAR(10),
    "country" CHAR(2),
    "cc2" VARCHAR(60),
    "admin1" VARCHAR(20),
    "admin2" VARCHAR(80),
    "admin3" VARCHAR(20),
    "admin4" VARCHAR(20),
    "population" BIGINT,
    "elevation" INTEGER,
    "dem" VARCHAR(200),
    "timezone" VARCHAR(40),
    "modification_date" DATE
);