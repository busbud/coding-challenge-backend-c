var pg = require('../utils/pgclient');

const QUERY_SELECT_SUGGESTION_BY_TERM = `
    SELECT name, ascii, lat AS latitude, long AS longitude, country, admin1
    FROM cities
    WHERE (name ILIKE '%' || $1 || '%' OR alt_name ILIKE '%' || $1 || '%')
      AND country IN ('CA','US')
      AND population > 5000
   `

module.exports = {
    getSuggestionByTerm: async function (term, lat, lon) {
        return await pg.select(QUERY_SELECT_SUGGESTION_BY_TERM, [term]);
    }
}