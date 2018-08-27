const log = require('../lib/logger')('models.cities');
const redis = require('../lib/redis').getClient();

const {
  applyDistanceScores,
  applyPopulationScores,
  computeFinalScoreAndFormat,
  getCitiesMatchingPrefix,
  normalizeSearchTerm,
} = require('./cities.helper.js');

const {getCities} = require('../data/citiesStore.js');

const redisPrefix = `bb:cache:suggestions`;

// Declare cities here so it's only loaded once
let cities = null;

module.exports = {
  async getSuggestions({q, latitude, longitude}) {
    cities = getCities();
    const selectedCities = [];
    if(q) {
      // Normalize the search query
      const normalizedQuery = normalizeSearchTerm(q);

      const cacheHit = await redis('get', `${redisPrefix}:${normalizedQuery}`);

      let citiesResult;
      if(cacheHit) {
        log.i(`Cache hit for query "${normalizedQuery}"!`);
        citiesResult = JSON.parse(cacheHit);
      } else {
        // Cities matching the normalized prefix, with name scores
        citiesResult = getCitiesMatchingPrefix(cities, normalizedQuery);
        // Add city population scores
        citiesResult = applyPopulationScores(citiesResult);

        await redis('setex', `${redisPrefix}:${normalizedQuery}`, 1 * 24 * 60 * 60, JSON.stringify(citiesResult)); // 1 day expiration
      }

      if(latitude) citiesResult = applyDistanceScores(citiesResult, {latitude, longitude});

      citiesResult = computeFinalScoreAndFormat(citiesResult);

      selectedCities.push(...citiesResult);
    }

    return selectedCities;
  },
};
