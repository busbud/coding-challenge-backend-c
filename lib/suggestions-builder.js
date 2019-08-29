/**
 * Building suggestions
 *
 * 1. Find all cities who's names are no more than MAX_DISTANCE different from `q` query
 * 2. Add calculated distance from caller's location to them if location provided
 * 3. Calculate score and sort by score
 *
 * Scoring factors (from major to minor):
 * [✓] Levenshtein distance between two words (shortened to shortest)
 * [✓] Distance from caller's location
 * [✓] Populations size (largest cities first)
 * [ ] Average position in cache (will pop up more popular cities)
 */

'use strict';

const levenshtein = require('fast-levenshtein');
const geolib = require('geolib');

const { CITIES } = require('./data-loader');

/**
 * Maximum number of suggestions per request
 */
const MAX_RESULTS = 20;

/**
 * Calculates scores for each suggestion while sorts the array in place
 * Out ideal, confident suggestion have Levenshtein distance 0,
 * closest to the caller and biggest in population
 * Maximum possible distance is globe diameter - 12 742 000 meters
 * Mutates passed object by adding score field
 * Our score weights: 60% to Levenstain distance, 10% to population size, and 30% to the distance from caller, if available
 *
 * @param {{ lev_distance: number, meters_distance?: number, population: BigInt }} city
 * @param {number} shortestLev - shortest Levenshtein of all results
 * @param {number} closestDistance - closest place distance in meters
 * @param {BigInt} largestPopulation - largest population of all result
 */
function calcCityScore(city, shortestLev, closestDistance, largestPopulation) {
  let score =
    ((shortestLev + 1) / (city.lev_distance + 1)) * 0.6 + // Levenshtein duration can be 0
    (city.population / largestPopulation) * 0.1;
  if (city.meters_distance)
    score += (closestDistance / city.meters_distance) * 0.3;
  city.score = Math.min(1, score); // ensure not cross 1
}

/**
 *
 * @param {import('koa').Context} ctx
 * @param {number} [latitude]
 * @param {number} [longitude]
 */
function buildSuggestions(ctx, latitude, longitude) {
  const query = ctx.query.q;

  let cities;

  // find all with Levenshtein distance <= MAX_DISTANCE, including alternative names
  // we will allow 1 misspelling per 5 letters of input. "Magic number", seems reasonable to me
  let closestDistance = Infinity;
  let shortestLev = Infinity;
  let largestPopulation = 0;

  // if call just added one more letter to previous results then we can use cached state
  // i.e. (1) a-> ab -> abc - all sequential calls will be reductions of previous results array
  if (query.startsWith(ctx.session.lastQuery)) {
    // we need to recalculate Levenshtein distance only on a previously filtered array
    cities = ctx.session.sessionResults.flatMap(city => {
      for (const n of [city.name, ...city.alt_name]) {
        // if city name is shorter than query then we will ignore it
        if (n.length < query.length) continue;

        // we will allow one misspelling for each 5 characters, "magic number", just common sense
        const MAX_DISTANCE = Math.floor(query.length / 5);
        // it's autoCOMPLETE, so we are looking to matches from start
        const dist = levenshtein.get(n.substr(0, query.length), query, {
          useCollator: true
        });
        if (dist <= MAX_DISTANCE) {
          const res = {
            ...city,
            query_match: n,
            lev_distance: dist
          };
          if (shortestLev > dist) shortestLev = dist;

          if (
            Number.isFinite(city.meters_distance) &&
            closestDistance > city.meters_distance
          )
            closestDistance = city.meters_distance;

          if (largestPopulation < city.population)
            largestPopulation = city.population;
          return res;
        }
      }
      // flatMap will exclude such elements from resulting array
      return [];
    });
  } else {
    // first time request o query changed from previous requests
    // Passing whole array, filtering out impossible cities,
    // saving shortest Levenshtein distance, closest distance and largest population
    cities = CITIES.flatMap(city => {
      for (const n of [city.name, ...city.alt_name]) {
        // if city name is shorter than query then we will ignore it
        if (n.length < query.length) continue;

        // we will allow one misspelling for each 5 characters, "magic number", just common sense
        const MAX_DISTANCE = Math.floor(query.length / 5);
        // it's autoCOMPLETE, so we are looking to matches from start
        const dist = levenshtein.get(n.substr(0, query.length), query, {
          useCollator: true
        });
        if (dist <= MAX_DISTANCE) {
          const res = {
            ...city,
            query_match: n,
            lev_distance: dist
          };
          if (shortestLev > dist) shortestLev = dist;

          if (latitude && longitude) {
            res.meters_distance = geolib.getDistance(
              { latitude, longitude },
              { latitude: city.latitude, longitude: city.longitude }
            );
            if (closestDistance > res.meters_distance)
              closestDistance = res.meters_distance;
          }

          if (largestPopulation < city.population)
            largestPopulation = city.population;
          return res;
        }
      }
      // flatMap will exclude such elements from resulting array
      return [];
    });
  }

  // add scores and sort
  cities.sort((city1, city2) => {
    calcCityScore(city1, shortestLev, closestDistance, largestPopulation);
    calcCityScore(city2, shortestLev, closestDistance, largestPopulation);
    return city2.score - city1.score;
  });

  // save calculation and sort results to session
  ctx.session.sessionResults = cities;

  // cut at maximum results and remove extra fields
  return cities
    .slice(0, MAX_RESULTS)
    .map(({ name, admin1, country, score, longitude, latitude }) => ({
      name: `${name}, ${admin1}, ${country}`,
      score,
      longitude,
      latitude
    }));
}

module.exports = buildSuggestions;
