// Internal Dependencies
var _ = require('../utils/underscore_with_string');
var logger = require('../utils/logger');
var utilMethods = require('../utils/util_methods');
var Constant = require('../config/constant');

// External dependencies
var string_score = require('string_score');


var scoringService = exports;


// Compute how much names match
function computeScoreForString(value1, value2) {
  var isVal1Blank = _.str.isBlank(value1);
  var isVal2Blank = _.str.isBlank(value2);
  if (isVal1Blank || isVal1Blank) {
    logger.debug('ScoringService#computeScoreForString -- ' +
      'calling methods with missing params');
    return 0;
  }

  var valueFormatted1 = utilMethods.getLowerCaseUnidecodedString(value1);
  var valueFormatted2 = utilMethods.getLowerCaseUnidecodedString(value2);

  var score = valueFormatted1.score(valueFormatted2);

  return score;

}

function computeScoreForCoordinates(coordinates1, coordinates2) {

  var isCoordinate1AnObject = _.isObject(coordinates1);
  if (!isCoordinate1AnObject) {
    logger.debug('ScoringService#computeScoreForCoordinates -- ' +
      'First param is not an object');
    return 0;
  }

  var isCoordinate2AnObject = _.isObject(coordinates2);
  if (!isCoordinate2AnObject) {
    logger.debug('ScoringService#computeScoreForCoordinates -- ' +
      'Second param is not an object');
    return 0;
  }

  var hasParam1PropertyLatitude = _.has(coordinates1, 'latitude');
  var hasParam1PropertyLongitude = _.has(coordinates1, 'longitude');

  var hasParam2PropertyLatitude = _.has(coordinates2, 'latitude');
  var hasParam2PropertyLongitude = _.has(coordinates2, 'longitude');

  var canProcessScoreComputation =
    hasParam1PropertyLatitude || hasParam1PropertyLongitude ||
    hasParam2PropertyLatitude || hasParam2PropertyLongitude;

  if (!canProcessScoreComputation) {
    logger.debug('ScoringService#computeScoreForCoordinates -- ' +
      'Seems that some values are missing');
    return 0;
  }

  var lat1 = coordinates1.latitude;
  var long1 = coordinates1.longitude;

  var lat2 = coordinates2.latitude;
  var long2 = coordinates2.longitude;

  // Get the haversine distance in kilometers
  var distanceInKmsBetweenPoints = utilMethods.haversineDistance(lat1, long1, lat2, long2);

  // Report value to 1 and substract
  // (because if we are close the divide result will be little and the real
  //  score has to be high)
  var score = 1 - (distanceInKmsBetweenPoints / Constant.EARTH_RADIUS_IN_KMS);

  return score;

}


scoringService.computeScoreForString = computeScoreForString;
scoringService.computeScoreForCoordinates = computeScoreForCoordinates;
