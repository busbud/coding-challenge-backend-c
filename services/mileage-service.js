// Internal dependencies
var _ = require('../utils/underscore_with_string');
var logger = require('../utils/logger');
var utilMethods = require('../utils/util_methods');
var Constant = require('../config/constant');

// External dependencies
var Q = require('q');


var mileageService = exports;


function computeDistance(opts) {

  return Q.fcall(function() {

    var isParamAnObject = _.isObject(opts);
    if (!isParamAnObject) {
      throw new Error('MileageService#computeDistance -- ' +
        'opts is not an object');
    }

    var hasDepartureParam = _.has(opts, 'departure');
    var hasArrivalParam = _.has(opts, 'arrival');
    if (!hasDepartureParam || !hasArrivalParam) {
      throw new Error('MileageService#computeDistance -- ' +
        'Opts malformed, missing departure or arrival object');
    }

    var departure = opts.departure;
    var arrival = opts.arrival;

    var hasDepartureLatitude = _.has(departure, 'latitude');
    var hasDepartureLongitude = _.has(departure, 'longitude');
    if (!hasDepartureLatitude || !hasDepartureLongitude) {
      throw new Error('MileageService#computeDistance -- ' +
        'Malformed departure object, missing latitude or longitude values');
    }

    var hasArrivalLatitude = _.has(arrival, 'latitude');
    var hasArrivalLongitude = _.has(arrival, 'longitude');
    if (!hasArrivalLatitude || !hasArrivalLongitude) {
      throw new Error('MileageService#computeDistance -- ' +
        'Malformed arrival object, missing latitude or longitude values');
    }

    var depLatitude = departure.latitude;
    var depLongitude = departure.longitude;
    var arrLatitude = arrival.latitude;
    var arrLongitude = arrival.longitude;

    // Get dist in fly bird kms
    var mileageInKms =
      utilMethods.haversineDistance(depLatitude, depLongitude,
        arrLatitude, arrLongitude);

    var mileageInMiles = mileageInKms * Constant.KM_TO_MILES;

    var response = {
      mileage: {
        kms: mileageInKms,
        miles: mileageInMiles
      }
    };

    return response;

  });
}



// Exposed functions
mileageService.computeDistance = computeDistance;
