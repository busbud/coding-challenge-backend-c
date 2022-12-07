const { suggestions, validateRequest } = require("./units");
module.exports = async ({ req, res }) => {
  const {
    query: { q = "", latitude = "", longitude = "" },
  } = req;

  validateRequest({ q, location: { latitude, longitude } });

  const results = await suggestions({ q, location: { latitude, longitude } });

  if (!results.suggestions.length) {
    return { status: 404, ...results };
  }

  return results;
};
