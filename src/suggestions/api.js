const scoreCitiesByRelevancy = require('./scoring').default;
const getCitiesByNameStartingWith = require('../database/api').default;


function buildApiResponse(cities) {
  const apiResponseCities = [];
  cities.forEach((city) => {
    apiResponseCities.push(city.toApiResponseObject());
  });
  return apiResponseCities;
}

/**
 * Function tasked with retrieving relevant City results from the database for the suggestions API
 */
export default function getSuggestions(nameQuery, lat = null, lon = null) {
  return new Promise((resolve) => {
    getCitiesByNameStartingWith(nameQuery).then((cities) => {
      // No results at all
      if (cities === undefined || cities.length === 0) {
        resolve([]);
      }
      // We have some results, let's order them by relevancy and limit the amount of results
      scoreCitiesByRelevancy(cities, nameQuery, lat, lon).then((scoredCities) => {
        const apiResponseCities = buildApiResponse(scoredCities);
        resolve(apiResponseCities);
      });
    });
  });
}
