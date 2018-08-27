const {
  applyDistanceScores,
  applyPopulationScores,
  computeFinalScoreAndFormat,
  getCitiesMatchingPrefix,
  normalizeSearchTerm,
} = require('./cities.helper.js');

module.exports = {
  getSuggestions({q, latitude, longitude}) {
    const selectedCities = [];
    if(q) {
      // Normalize the search query
      const normalizedQuery = normalizeSearchTerm(q);

      // Cities matching the normalized prefix, with name scores
      let cities = getCitiesMatchingPrefix(normalizedQuery);
      // Add city population scores
      cities = applyPopulationScores(cities);

      // TODO: cache result here (in-memory? Redis?)

      if(latitude && longitude) cities = applyDistanceScores(cities, {latitude, longitude});

      cities = computeFinalScoreAndFormat(cities);

      selectedCities.push(...cities);
    }

    return selectedCities;
  },
};
