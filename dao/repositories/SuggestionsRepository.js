/* eslint-disable comma-dangle */
// Utilities
const PG = require('../../utilties/Postgres');

// Service
const ErrorService = require('../../services/ErrorService');


module.exports = {
  getSuggestions: async (search, options = { returnSQL: false }) => {
    const { q } = search;
    const suggestions = `select json_agg(row_to_json(suggestions)) as suggestions from (select concat_ws(', ', ascii, admin1, country) as name, latitude, longitude from busbud m where m.name ~* '${q}' or m.alt_name ~* '${q}' and m.population > 5000) suggestions`;

    if (options.returnSQL) {
      return suggestions.toString();
    }

    try {
      let suggestionsResult = await PG.query(suggestions);
      suggestionsResult = (suggestionsResult.rows[0].suggestions) ? suggestionsResult.rows[0] : [];
      const result = { suggestions: suggestionsResult };
      return result;
    } catch (error) {
      throw new ErrorService.PgError(error);
    }
  },
};
