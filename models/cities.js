const {
  applyDistanceScores,
  applyPopulationScores,
  computeFinalScoreAndFormat,
  getCitiesMatchingPrefix,
  normalizeSearchTerm,
} = require('./cities.helper.js');

const {getCities} = require('../data/citiesStore.js');

// Declare cities here so it's only loaded once
let cities = null;

module.exports = {
  getSuggestions({q, latitude, longitude}) {
    cities = getCities();
    const selectedCities = [];
    if(q) {
      // Normalize the search query
      const normalizedQuery = normalizeSearchTerm(q);

      // Cities matching the normalized prefix, with name scores
      let citiesResult = getCitiesMatchingPrefix(cities, normalizedQuery);
      // Add city population scores
      citiesResult = applyPopulationScores(citiesResult);

      // TODO: cache result here (in-memory? Redis?)

      if(latitude) citiesResult = applyDistanceScores(citiesResult, {latitude, longitude});

      citiesResult = computeFinalScoreAndFormat(citiesResult);

      selectedCities.push(...citiesResult);
    }

    return selectedCities;
  },
};
