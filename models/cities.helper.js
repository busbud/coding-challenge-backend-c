const config = require('config');
const geolib = require('geolib');
const stringScore = require('string-score');
const {remove: removeDiacritics} = require('diacritics');
const weightedMean = require('../lib/weightedMean');

exports.buildClientCityEntry = function buildClientCityEntry(city) {
  return {
    id: city.id,
    name: city.displayName,
    longitude: city.long,
    latitude: city.lat,
    score: city.score,
  };
};

exports.normalizeSearchTerm = function normalizeSearchTerm(q) {
  return removeDiacritics(q)                        // Remove diacritics
  .toLowerCase()                                    // Convert to lower case
  .replace(/^st-/gi, 'st. ')                        // Replace "st-" with "st. " to match the format of the data
  .replace(/^st\.([A-Za-z]{1})(.*)/gi, 'st. $1$2'); // Add the space after the dot if it's missing (eg. st.john -> st. john)
};

exports.getCitiesMatchingPrefix = function getCitiesMatchingPrefix(cities, q) {
  // Increase stringScore fuzziness as string length increases
  // (users are more likely to be searching for a specific city with misspellings)
  const fuzziness = q.length < 5 ? 0.01 : q.length >= 10 ? 0.85 : q.length / 10;
  // Searches city names
  const selectedCities = [];
  for(let i = 0; i < cities.length; i++) {
    const scoreString = stringScore(cities[i].ascii, q, fuzziness);
    if(cities[i].ascii.startsWith(q) || scoreString > 0.5) {
      selectedCities.push({
        ...cities[i],
        scorePrefix: config.scoring.scores.name,
        scoreString,
      });
    } else {
      // If the city name doesn't match, we look at the alternate names
      for(let j = 0; j < cities[i].alt_name.length; j++) {
        if(cities[i].alt_name[j].toLowerCase().startsWith(q)) {
          selectedCities.push({
            ...cities[i],
            scorePrefix: config.scoring.scores.alt_name,
            scoreString: stringScore(q, cities[i].alt_name[j], 0.5),
          });
          break;
        }
      }
    }
  }

  return selectedCities;
};

exports.applyPopulationScores = function applyPopulationScores(cities) {
  // Sort by population
  return cities
  .sort((a, b) => a.population > b.population ? -1 : a.population === b.population ? 0 : 1)
  .map(city => ({...city, scorePopulation: city.population / cities[0].population}));
};

exports.applyDistanceScores = function applyDistanceScores(cities, location) {
  for(const city of cities) {
    city.distance = geolib.getDistance(
      {latitude: city.lat, longitude: city.long},
      location,
      100, // Precision of 100 meters should be sufficient
    );
  }

  // Sort by distance
  return cities
  .sort((a, b) => a.distance > b.distance ? 1 : a.distance === b.distance ? 0 : -1)
  .map(city => ({...city, scoreDistance: 1 - (1 - (cities[0].distance / city.distance))}));
};

exports.computeFinalScoreAndFormat = function computeFinalScoreAndFormat(cities) {
  const perfectStringMatchFound = !!cities.find(city => city.scorePrefix === 1);
  return cities
  .map(city => {
    // If some city names directly match, make alternate name matches score much much lower
    if(perfectStringMatchFound && city.scorePrefix === config.scoring.scores.alt_name) city.scorePrefix *= 0.05;

    const scoresArray = [
      [city.scorePrefix, config.scoring.weights.prefix],
      [city.scoreString, config.scoring.weights.string],
      [city.scorePopulation, config.scoring.weights.population],
    ];

    if(city.distance) scoresArray.push([city.scoreDistance, config.scoring.weights.distance]);

    return {
      ...city,
      score: parseFloat(weightedMean(scoresArray).toFixed(5)),
    };
  })
  .sort((a, b) => a.score > b.score ? -1 : a.score === b.score ? 0 : 1) // Order by score
  .slice(0, config.maxSuggestions) // Slice the array
  .map(city => exports.buildClientCityEntry(city)); // Format for output
};
