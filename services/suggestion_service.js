var removeDiacritics = require('diacritics').remove;
var _ = require('lodash');
var geolib = require('geolib');

var City = require('../models/city');

var SuggestionService = {};
SuggestionService.DEFAULT_DECIMALS = 3;

const NOT_PERFECT_MATCH_WEIGHT = .1;

const MISSING_LETTERS_MATCH_WEIGHT = .4;
const NAME_MISSING_LETTERS_MIN_THRESHOLD = 3;
const NAME_MISSING_LETTERS_MAX_THRESHOLD = 7;

const DISTANCE_WEIGHT = .25;
const DISTANCE_MIN_THRESHOLD = 1000;
const DISTANCE_MAX_THRESHOLD = 5000;

const POPULATION_WEIGHT = .1;
const POPULATION_MIN_THRESHOLD = 25000;
const POPULATION_MAX_THRESHOLD = 250000;

/**
 * Computes an adaptive score offset to be substracted from total score.
 *
 * @method computeAdaptiveWeight
 * @param {Number} value value of the candidate's score
 * @param {Number} weight the reference weight for this criteria, i.e. this will be also the maximum possible weight to be substracted from the candidate's score
 * @param {Number} threshold if the computed weight does beyond this threshold, the reference weight will be applied instead
 * @param {Function} minOrMax Math.min or Math.max
 * @return {Number} positive value to be substracted to target score, to lower its confidence
 */
var computeAdaptiveWeight = function(value, weight, threshold, minOrMax) {
  var currentWeight = (value * weight) / threshold;
  currentWeight = minOrMax(weight, currentWeight);
  return Math.abs(weight);
}

/**
 * Some fuzzy scoring logic for the coding challenge!
 * Score (or confidence) is altered according to (from strongest to weakest):
 * - Query text vs. city name: if there is less letters;
 * - (Optional) Distance from provided coordinates;
 * - Population.
 * 
 * @method computeScores
 * @param {Object} args an object
 * @param {String} args.q sanitized input query
 * @param {Number} args.latitude latitude (optional)
 * @param {Number} args.longitude longitude (optional)
 * @return {Array} Returns a hashtable, where the key is a city, and the value is its score.
 */
SuggestionService.computeScores = function(args, cities) {
  var criteriaScoreMap = {};
  _.forEach(cities, function(city) {
    var score = 1.0;

    // deal with name
    var query = args.q.toLowerCase();
    var cityName = city[City.ASCII_FIELD].toLowerCase();

    // Deal with city name
    var missingLetters = cityName.length - query.length;
    // if not perfect match
    if (missingLetters !== 0) {
      score -= NOT_PERFECT_MATCH_WEIGHT;
      // if it exceeds the threshold
      if (missingLetters >= NAME_MISSING_LETTERS_MIN_THRESHOLD) {
        score -= computeAdaptiveWeight(missingLetters, MISSING_LETTERS_MATCH_WEIGHT, NAME_MISSING_LETTERS_MAX_THRESHOLD, Math.min)
      }
    }

    // (Optional) Deal with distance
    var latitude = args.latitude;
    var longitude = args.longitude;
    if (!isNaN(latitude) && !isNaN(longitude)) {
      // https://www.npmjs.org/package/geolib
      var d = geolib.getDistance(
        { latitude: city[City.LATITUDE_FIELD], longitude: city[City.LONGITUDE_FIELD] },
        { latitude: Number(latitude), longitude: Number(longitude) }
      );
      // Under 1km, we do not bother
      // But over:
      if (d > DISTANCE_MIN_THRESHOLD) {
        score -= computeAdaptiveWeight(d, DISTANCE_WEIGHT, DISTANCE_MAX_THRESHOLD, Math.min)
      }
    }

    // population
    var population = city[City.POPULATION_FIELD];
    if (population < POPULATION_MAX_THRESHOLD) {
      score -= computeAdaptiveWeight(population, POPULATION_WEIGHT, POPULATION_MIN_THRESHOLD, Math.max);
    }

    criteriaScoreMap[city] = Math.max(0, score);
  });

  return criteriaScoreMap;
};

module.exports = SuggestionService;
