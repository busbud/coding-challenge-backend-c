var removeDiacritics = require('diacritics').remove;
var _ = require('lodash');
var geolib = require('geolib');

var City = require('../models/city');

var B1              = parseInt(1, 2);

// Flags
var EXACT_MATCH     = B1;
var STARTS_WITH     = B1 << B1;
var LESS_THAN_1_KM  = B1 << parseInt(2, 2);
var LESS_THAN_5_KM  = B1 << parseInt(3, 2);
var LESS_THAN_10_KM = B1 << parseInt(4, 2);
// Weight
var WEIGHTS = {
  EXACT_MATCH: 1000,
  STARTS_WITH: 500,
  LESS_THAN_1_KM: 250,
  LESS_THAN_5_KM: 125,
  LESS_THAN_10_KM: 75
};

var SuggestionService = {};
SuggestionService.WEIGHTS = WEIGHTS;

SuggestionService.computeAbsoluteScoreMap = function(args, cities) {
  var criteriaScoreMap = {};
  _.forEach(cities, function(city) {
    var score = 0;

    var query = args.q.toLowerCase();
    var cityName = city.ascii.toLowerCase();

    // if city name matches exactly
    if (query === cityName) score |= EXACT_MATCH;

    // if city name starts with
    if (cityName.indexOf(query) === 0) score |= STARTS_WITH;

    // distance from provided coordinates (optional)
    var latitude = args.latitude;
    var longitude = args.longitude;
    if (!isNaN(latitude) && !isNaN(longitude)) {
      // https://www.npmjs.org/package/geolib
      var d = geolib.getDistance(
        { latitude: city[City.LATITUDE_FIELD], longitude: city[City.LONGITUDE_FIELD] },
        { latitude: Number(latitude), longitude: Number(longitude) }
      );
      if (d < 1000) score |= LESS_THAN_1_KM;
      if (d < 5000) score |= LESS_THAN_5_KM;
      if (d < 10000) score |= LESS_THAN_10_KM;
    }

    criteriaScoreMap[city] = SuggestionService.computeScore(score);
  });

  return criteriaScoreMap;
};

SuggestionService.getHighestScore = function(criteriaScoreMap) {
    // get all score values in an array
    var scores = [];
    for (var key in criteriaScoreMap) {
      scores.push(criteriaScoreMap[key]);
    }
    // get highest score
    return _.max(scores);
}

SuggestionService.computeScore = function(binaryScore) {
  var score = 0;

  if (binaryScore & EXACT_MATCH) score += WEIGHTS.EXACT_MATCH;
  else {
    if (binaryScore & STARTS_WITH) score += WEIGHTS.STARTS_WITH;
  }

  if (binaryScore & LESS_THAN_1_KM) score += WEIGHTS.LESS_THAN_1_KM;
  if (binaryScore & LESS_THAN_5_KM) score += WEIGHTS.LESS_THAN_5_KM;
  if (binaryScore & LESS_THAN_10_KM) score += WEIGHTS.LESS_THAN_10_KM;

  return score;
}

module.exports = SuggestionService;
