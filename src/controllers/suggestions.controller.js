const { suggestionsQueries } = require('../services');

exports.get = async (req, res, next) => {
  try {
    const { query } = req;
    const suggestions = await suggestionsQueries.getSuggestions(query);

    if (!suggestions?.length) {
      res.status(404);
    }

    res.json({ suggestions });
  } catch (err) {
    next(err);
  }
};
