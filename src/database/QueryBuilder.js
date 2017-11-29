export default class QueryBuilder {
  build = params => {
    console.log('params', params);
    if (params && params.q && params.latitude && params.longitude) {
      return {
        sql:
          'SELECT ASCIINAME AS NAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE, levenshtein(LOWER($1), LOWER(NAME)) as SCORE, GREATEST(char_length(NAME), char_length($1)) as length FROM GEONAME WHERE NAME like $2 ORDER BY POINT(latitude, longitude) <-> POINT($3, $4) ASC',
        params: [params.q, params.q + '%', params.latitude, params.longitude]
      };
    } else if (params && params.latitude && params.longitude) {
      return {
        sql:
          'SELECT ASCIINAME AS NAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE FROM GEONAME ORDER BY POINT(latitude, longitude) <-> POINT($1, $2) ASC',
        params: [params.latitude, params.longitude]
      };
    } else if (params && params.q) {
      return {
        sql:
          'SELECT ASCIINAME AS NAME, ADMIN1 as REGION, COUNTRY, LATITUDE, LONGITUDE, levenshtein(LOWER($1), LOWER(NAME)) as SCORE, GREATEST(char_length(NAME), char_length($1)) as length FROM GEONAME WHERE lower(unaccent(NAME)) ILIKE lower(unaccent($2))',
        params: [params.q, params.q + '%']
      };
    } else {
      throw `unable to build query with params ${params}`;
    }
  };
}
