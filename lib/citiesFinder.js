const levenshtein = require('js-levenshtein');
const geolib = require('geolib');

const City = require('../models/city');


const GEO_RADIUS = 1000 * 1000;  // 1000 km

const NAME_MAX_RELATIVE_SCORE = 0.6;
const GEO_MAX_RELATIVE_SCORE = 0.4;
const MAX_SCORE = NAME_MAX_RELATIVE_SCORE + GEO_MAX_RELATIVE_SCORE;

/**
 * Uses Levenshtein distance in order to scoring name differences
 *
 * @param {string} source
 * @param {string} target
 * @returns {number} The name score
 */
function computeNameScore (source, target) {
  const targetLength = target.length;
  const distance = levenshtein(source.toLowerCase(), target.toLowerCase());

  // Compute score equal to a Levenshtein ratio
  if (distance > targetLength) {
    return 0;
  }
  return 1 - distance / targetLength;
}

/**
 * Compute a geo score according to the distance between two location and a defined radius
 *
 * @param {{latitude: number, longitude: number}} source
 * @param {{latitude: number, longitude: number}} target
 * @returns {number} The name score
 */
function computeGeoScore (source, target) {
  const distance = geolib.getDistance(source, target);
  if (distance > GEO_RADIUS) {
    return 0;
  }
  return 1 - (distance / GEO_RADIUS);
}

/**
 * Compute a global score. If a geo score exists, we need to use a relative max score
 * representing a kind of score weight.
 *
 * @param {number} nameScore
 * @param {number|null} geoScore
 * @returns {number}
 */
function computeGlobalScore (nameScore, geoScore = null) {
  if (geoScore !== null) {
    nameScore = nameScore * NAME_MAX_RELATIVE_SCORE / MAX_SCORE;
    geoScore = geoScore * GEO_MAX_RELATIVE_SCORE / MAX_SCORE;
  }
  const score = (nameScore) + (geoScore || 0);
  return Math.round(score * 10) / 10;
}

/**
 * Compute a score for each city thanks to function arguments name and coordinates.
 * We add a the score for each relative city and we make some processing
 * (like name processing).
 *
 * @param {Array} cities
 * @param {string} name
 * @param {{latitude: number, longitude: number}} coordinates
 * @returns {Array}
 */
function processCities (cities, name, coordinates = {}) {
  let processedCities = [];
  for (const city of cities) {
    // Name score computation
    const nameScore = computeNameScore(city.name, name);

    // Geo score computation
    let geoScore = null;
    if (
      Object.keys(coordinates).includes('latitude') &&
      Object.keys(coordinates).includes('longitude')
    ) {
      const targetCoordinates = {
        latitude: city.lat,
        longitude: city.long,
      };
      geoScore = computeGeoScore(coordinates, targetCoordinates);
    }

    // Process city with wanted data
    processedCities = [
      ...processedCities,
      {
        name: `${city.name}, ${city.admin_division_code}, ${city.country}`,
        latitude: city.lat,
        longitude: city.long,
        score: computeGlobalScore(nameScore, geoScore),
      }
    ];
  }
  return processedCities;
}

/**
 * Get cities from database and get sorted relevant cities. We do not need cities
 * with a score of 0.
 *
 * @param {string} name
 * @param {Object} coordinates
 * @param {number} limit
 * @returns {Promise<Array>}
 */
async function getRelevantCities (name, coordinates = {}, limit = 4) {
  const cities = await City.findAll();
  const processedCities = processCities(cities, name, coordinates);
  const filteredCities = processedCities.filter(city => city.score > 0);
  const sortedCities = filteredCities.sort((a, b) => b.score - a.score);
  return sortedCities.slice(0, limit);
}

module.exports = {getRelevantCities};