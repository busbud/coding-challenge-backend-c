/**
 * Required Modules.
 */
const { filterCities } = require('../helpers');

/**
 * Service to get a list of suggestions.
 *
 * @param {Object} Object       Object used as an interface to get some properties.
 * @param {Object} Object.query The object containing the query strings to perform the filter.
 * @return {Array<Object>}      An array of objects.
 */
const getSuggestionsService = ({ query }) => {
  try {
    // Create the suggestions based on the query filters.
    const suggestions = filterCities({
      cities: global.cities || [],
      name: query.q,
      latitude: Number(query.latitude),
      longitude: Number(query.longitude),
    });

    return suggestions;
  } catch (error) {
    const errMsg =
      'There was an error getting suggestions (service > getSuggestionsService).';
    console.error(`${errMsg} ${JSON.stringify(error)}`);
    throw error;
  }
};

/**
 * Export the service methods.
 */
module.exports = {
  getSuggestionsService,
};
