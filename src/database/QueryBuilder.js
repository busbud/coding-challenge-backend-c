export default class QueryBuilder {
  build = params => {
    if (params && params.q && params.latitude && params.longitude) {
      return {
        sql:
          'SELECT ASCIINAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE, scoring_latlng_name(LOWER($1), LOWER(ASCIINAME), LATITUDE::numeric, LONGITUDE::numeric, $3::numeric, $4::numeric) as SCORE FROM GEONAME WHERE ASCIINAME ILIKE lower(unaccent($2)) ORDER BY SCORE DESC',
        params: [params.q, params.q + '%', params.latitude, params.longitude]
      };
    } else if (params && params.latitude && params.longitude) {
      return {
        sql:
          'SELECT ASCIINAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE, scoring_latlng(LATITUDE::numeric, LONGITUDE::numeric, $1::numeric, $2::numeric) as SCORE FROM GEONAME ORDER BY SCORE DESC',
        params: [params.latitude, params.longitude]
      };
    } else if (params && params.q) {
      return {
        sql:
          'SELECT ASCIINAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE, levenshtein(LOWER($1), LOWER(ASCIINAME)) as SCORE FROM GEONAME WHERE ASCIINAME ILIKE lower(unaccent($2)) ORDER BY SCORE DESC',
        params: [params.q, params.q + '%']
      };
    } else {
      throw `unable to build query with params ${params}`;
    }
  };
}
