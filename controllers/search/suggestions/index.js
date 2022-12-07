const { suggestions, validateRequest } = require("./units");
const { error } = require("../../../lib");
module.exports = async ({ req, res }) => {
  const {
    query: { q = "", latitude = "", longitude = "" },
  } = req;

  validateRequest({ q, location: { latitude, longitude } });

  const results = await suggestions({ q, location: { latitude, longitude } });

  if (!results.suggestions.length) {
    // error("NotFound", "Could not find any match", 404, results);
    return { status: 404, ...results };
  }

  return results;
};
