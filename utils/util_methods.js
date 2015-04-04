// Internal dependencies
var _ = require('./underscore_with_string');
var logger = require('../utils/logger');
var Constant = require('../config/constant');

// External dependencies
var unidecode = require('unidecode');


var utilMethods = exports;


function getLowerCaseUnidecodedString(value) {

  var isParamBad = _.str.isBlank(value);
  if (isParamBad) {
    return value;
  }

  var lowerCaseValue = value.toLowerCase();
  var unidecodedLowerCaseValue = unidecode(lowerCaseValue);

  return unidecodedLowerCaseValue;

}

function unidecodeString(value) {

  var isParamBad = _.str.isBlank(value);
  if (isParamBad) {
    return value;
  }

  var unidecodedValue = unidecode(value);

  return unidecodedValue;

}



///////////////////
//
// Using an existing distance comparison called haversine
// but I won't use the earth rayon because distance in km doesn't interest me
//
////////////////////////////////


function haversineDistance(lat1, lon1, lat2, lon2) {

  var floatValueLat1 = parseFloat(lat1);
  var floatValueLon1 = parseFloat(lon1);
  var floatValueLat2 = parseFloat(lat2);
  var floatValueLon2 = parseFloat(lon2);

  var areAllValuesNumber = !!floatValueLat1 &&
    !!floatValueLon1 &&
    !!floatValueLat2 &&
    !!floatValueLon2;

  if (!areAllValuesNumber) {
    throw new Error(
      'UtilMethods#haversineDistance -- Only number are allowed');
  }

  var x1 = lat2 - lat1;
  var dLat = toRad(x1);

  var x2 = lon2 - lon1;
  var dLon = toRad(x2);

  var a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
    Math.cos(toRad(lat1)) * Math.cos(toRad(lat2)) *
    Math.sin(dLon / 2) * Math.sin(dLon / 2);

  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));

  var d = Constant.EARTH_RADIUS_IN_KMS * c;

  return d;

}

function toRad(value) {
  return (value * Math.PI / 180);
}



utilMethods.getLowerCaseUnidecodedString = getLowerCaseUnidecodedString;
utilMethods.unidecodeString = unidecodeString;

utilMethods.haversineDistance = haversineDistance;
