const Cache = require('./utils/cache');
const dataFile = require('./utils/dataFile');
const distance = require('./utils/distance');
const geoCode = require('./utils/geoCode');
const stringNormalization = require('./utils/stringNormalization');

let file;
let cities;
let cache;

/**
 * Search term in all the cities
 * @param {string} term Term to search in cities file
 * @param {number} limit Limit the number of results
 * @param {number} lat Latitude to search
 * @param {number} long Longitude to search
 */
function search(term, limit, lat, long) {
  if (cities === undefined || cities.length === 0) {
    return 'Cities undefined';
  }

  const results = [];
  const filteredResults = [];

  cities.forEach((city) => {
    const name = city.ascii;
    let country;

    switch (city.country) {
      case 'CA':
        country = 'Canada';
        break;
      case 'US':
        country = 'USA';
        break;
      default:
        country = 'N/A';
    }

    const admin = city.country === 'CA' ? geoCode.getCanadianCode(city.admin1) : city.admin1;

    const score = distance.get(
      stringNormalization.normalize(term),
      stringNormalization.normalize(name).replace(/\./g, '').replace(/,/g, ''),
      lat,
      long,
      city.lat,
      city.long,
    );

    // Do not use cities outside CA/US and null score
    if (country !== 'N/A' && score >= 0.1) {
      results.push({
        name: `${name}, ${admin}, ${country}`,
        latitude: city.lat,
        longitude: city.long,
        score,
      });
    }
  });

  if (results.length === 0) {
    return [];
  }

  // Sorting the results by descending score
  results.sort((a, b) => b.score - a.score);

  // Limit the result to X (=limit) cities
  for (let i = 0; i < limit; i += 1) {
    filteredResults.push(results[i]);
  }

  return filteredResults;
}

/**
 * Read cities from file or cache and run search with specific term
 * @param {string} term Term to search in cities file
 * @param {number} limit Limit the number of results
 * @param {number} lat Latitude searched by user
 * @param {number} long Longitude searched by user
 */
module.exports.getElements = (term, limit, lat, long) => {
  if (cache === undefined) {
    cache = new Cache(10, '#');
  }

  return new Promise(((resolve, reject) => {
    const key = cache.getCacheKey(term, limit, lat, long);

    if (cache.isSearchInCache(key)) {
      resolve(cache.getSearchFromCache(key));
    }

    file = dataFile.import();

    file.then((result) => {
      cities = result;

      const searchResult = search(term, limit, lat, long);
      cache.addSearchToCache(key, searchResult);
      resolve(searchResult);
    }, (err) => {
      reject(err);
    });
  }));
};
