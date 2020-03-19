var pg = require('../utils/pgclient');

const QUERY_SELECT_SUGGESTION_BY_TERM = `
    SELECT name, country, admin1, population
    FROM cities
    WHERE name ILIKE '%' || $1 || '%'
    OR alt_name ILIKE '%' || $1 || '%'
   `

module.exports = {
    getSuggestionByTerm: async function (term, lat, lon) {
        return await pg.select(QUERY_SELECT_SUGGESTION_BY_TERM, [term]);
    }
}