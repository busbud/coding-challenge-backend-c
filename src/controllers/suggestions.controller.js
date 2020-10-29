/**
 * Required Modules.
 */
const url = require('url');

const { errorHandler } = require('../helpers');
const { getSuggestionsService } = require('../services');

/**
 * Get a list of suggestions.
 *
 * @param {Object} req      The HTTP request object.
 * @param {Object} res      The HTTP response object.
 * @return {Array<Object>}  An array of objects.
 */
const getSuggestions = (req, res) => {
  try {
    const { query } = url.parse(req.url, true);

    // Instantiate the service to get the list of suggestions.
    const suggestions = getSuggestionsService({ query });

    // Default status code for an empty array.
    let statusCode = 404;
    if (suggestions.length > 0) {
      // Set the status for records found.
      statusCode = 200;

      // Sort suggestions by descending score.
      suggestions.sort((a, b) => b.score - a.score);
    }

    res.statusCode = statusCode;
    res.end(JSON.stringify({ suggestions }));
  } catch (error) {
    errorHandler(error, res);
  }
};

/**
 * Export the controller methods.
 */
module.exports = {
  getSuggestions,
};
