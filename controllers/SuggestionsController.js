// Dao
const SuggestionsDao = require('../dao/SuggestionsDao');

// ErrorService
const { MissingRequestParameter } = require('../services/ErrorService');

module.exports = {
  async getSuggestions(ctx) {
    const urlQuery = ctx.query;
    const { q } = urlQuery;

    if (!q) {
      throw new MissingRequestParameter('q');
    }

    const result = await SuggestionsDao.getSuggestions(ctx, urlQuery);

    const { suggestions } = result;

    return { suggestions };
  },
};
