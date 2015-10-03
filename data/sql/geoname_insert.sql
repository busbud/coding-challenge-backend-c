INSERT into geoname (
  geonameid,
  name,
  asciiname,
  alternatenames,
  latitude,
  longitude,
  feature_class,
  feature_code,
  country_code,
  cc2,
  admin1_code,
  admin2_code,
  admin3_code,
  admin4_code,
  population,
  elevation,
  dem,
  tz,
  mod_date
) VALUES (
  $1,
  $2,
  $3,
  $4,
  $5,
  $6,
  $7,
  $8,
  $9,
  $10,
  $11,
  $12,
  $13,
  $14,
  $15,
  $16,
  $17,
  $18,
  $19
);