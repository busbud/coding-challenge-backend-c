const { remove: removeDiacritics } = require('diacritics');
const metricLcs = require('metric-lcs');
const Sequelize = require('sequelize');
const weightedMean = require('weighted-mean');
const { City } = require('./models');
const fips = require('./fips');
const { client: redis } = require('./redis');


// Database and Redis queries

async function queryCities(latitude = null, longitude = null) {
  const attributes = ['name', 'countryCode', 'latitude', 'longitude', 'population', 'admin1Code'];

  // If the user provides lat/long, calculate the distance via PostGIS (in metres).
  if (latitude && longitude) {
    attributes.push([
      Sequelize.fn('ST_DistanceSphere',
        Sequelize.col('geom'),
        Sequelize.fn('ST_SetSRID',
          Sequelize.fn('ST_Point', longitude, latitude), 4326)),
      'distance',
    ]);
  }

  return City.findAll({ attributes, raw: true });
}

function buildRedisKey(query, latitude = null, longitude = null) {
  let key = `ac:cacheq:${query}`;
  if (latitude && longitude) {
    key += `:${parseFloat(longitude).toFixed(1)}:${parseFloat(latitude).toFixed(1)}`;
  }
  return key;
}

async function loadFromCache(query, latitude = null, longitude = null) {
  const key = buildRedisKey(query, latitude, longitude);
  return new Promise((resolve, reject) => {
    redis.get(key, (err, cities) => {
      if (err) reject(err);
      resolve(cities);
    });
  });
}

function saveToCache(query, cities, latitude = null, longitude = null) {
  const key = buildRedisKey(query, latitude, longitude);
  redis.expire(key, 300);
  redis.set(key, JSON.stringify(cities));
}


// Cleaning, filtering and sorting

function sanitizeValue(value) {
  // Transform a value to lowercase and remove diactitics.
  return removeDiacritics(value).toLowerCase();
}

function getRankOfRange({ min, max, number, weight = 1 }) {
  return ((number - min) / Math.abs(max - min)) * weight;
}

function compareCity(a, b) {
  if (a.score === b.score) return 0;
  if (a.score < b.score) return 1;
  return -1;
}


// Ranking and filtering

function rankCity(city, query, ranges = {}) {
  const parsedCity = sanitizeValue(city.name);
  const fullName = `${city.name}, ${city.countryCode === 'CA' ? fips[city.countryCode][city.admin1Code] : city.admin1Code}`;

  /**
    Calculate scores for different comparisons.
    Prefix: if the user enters a string that is the direct prefix of a city, it's
      highly scored.
    String: we look for the longest common substring (LCS) score of the text match.
    Distance: improve the search results by preferring closer results vs. further ones.
      Only applicable if the user provides lat/long.
    Population: If results are very similar, we tend to prefer larger cities over smaller ones.
  */
  const scores = {
    prefix: parsedCity.startsWith(query) ? 1 : 0,
    string: metricLcs(query, parsedCity),
    distance: ranges.distance
      ? (1 - getRankOfRange({ ...ranges.distance, number: city.distance })) : 0,
    population: ranges.population
      ? getRankOfRange({ ...ranges.population, number: city.population }) : 0,
  };

  // Weight the individual scores before calculating the average, "overall" score.
  const allScores = [[scores.prefix, 1], [scores.string, 0.75], [scores.population, 0.5]];
  if (ranges.distance) {
    allScores.push([scores.distance, 1]);
  }
  scores.overall = parseFloat(weightedMean(allScores).toFixed(3));

  return { ...city, name: fullName, scores };
}

function rankCities(cities, query, hasDistances = false) {
  const ranges = {};

  // Get the smallest and largest populations and distances in the dataset.
  // This is used for creating our city rankings in the next step.
  const populations = cities.map(city => city.population);
  ranges.population = { min: Math.min(...populations), max: Math.max(...populations) };
  if (hasDistances) {
    const distances = cities.map(city => city.distance);
    ranges.distance = { min: Math.min(...distances), max: Math.max(...distances) };
  }

  return cities.map(city => rankCity(city, query, ranges));
}

function filterCities(cities) {
  // Only show the top 10 results above 0.25.
  // Also, if the string match is less than 0.33, don't bother.
  const filteredCities = cities
    .filter(city => city.scores.string >= 0.33)
    .filter(city => city.scores.overall >= 0.25);

  filteredCities.forEach((city) => {
    const newCity = city;
    newCity.score = newCity.scores.overall;

    // Hide results that we don't need.
    delete newCity.scores;
    delete newCity.admin1Code;
  });
  return filteredCities.sort(compareCity).slice(0, 10);
}


module.exports = {
  queryCities, rankCities, filterCities, saveToCache, loadFromCache, sanitizeValue,
};
