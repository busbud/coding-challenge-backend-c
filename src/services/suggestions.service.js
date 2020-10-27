/**
 * Required Modules.
 */
const { filterCities } = require('../helpers');
const { readAndParseCSVFile } = require('../libs');

/**
 * Service to get a list of suggestions.
 *
 * @param {Object} Object       Object used as an interface to get some properties.
 * @param {String} Object.file  The file to be parsed.
 * @param {Object} Object.query The object containing the query strings to perform the filter.
 * @return {Array<Object>}      An array of objects.
 */
const getSuggestionsService = async ({ file, query }) => {
  // Read and parse the .csv file.
  const cities = await readAndParseCSVFile({ file });

  // Create the suggestions based on the query filters.
  const suggestions = filterCities({
    cities,
    name: query.q,
    latitude: Number(query.latitude),
    longitude: Number(query.longitude),
  });

  return suggestions;
};

/**
 * Export the service methods.
 */
module.exports = {
  getSuggestionsService,
};
