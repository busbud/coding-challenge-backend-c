/**
 * GeoPoint class
 *
 * Represents a point on the globe (latitude / longitude)
 *
 */
var util = require('util');
var _ = require('lodash');

var InvalidParameterError = require('./errors').InvalidParameterError;

function toRadian(num) {
  return num * Math.PI / 180;
}

var coordinateLimit = {latitude: 90, longitude: 180};

/**
 * private function that checks
 * if latitude or longitude values are valid (and in bound)
 *
 * @param a float (Number) 
 * @param  String - 'latitude' || 'longitude'
 * @return Boolean
 */
function isValidCoord (val, type) {
  var limit = coordinateLimit[type];
  return !(!isNaN(val) && val !== false && val >= -limit && val <= limit);
}

/**
 * @param lat - latitude in degree
 * @param lat - latitude in degree
 * @return Point
 */
function Point(lat, lng) {
  this.lat = !isNaN(lat) && parseFloat(lat);
  this.lng = !isNaN(lng) && parseFloat(lng);
  _.each({
    latitude: this.lat,
    longitude: this.lng
  }, function (val, type) {
    if (isValidCoord(val, type)) {
      throw new InvalidParameterError(util.format('invalid %s (%s)', type, val));
      // stop here (thanks lodash)
      return false;
    }
  });
}

_.extend(Point.prototype, {

  /** 
   * Source: https://github.com/manuelbieh/Geolib/blob/master/src/geolib.js#L370
   * I didn't like the Number.prototype modification (for .toRad())
   * I should the simple calculation method because 
   * there is no need to be precise for long distance
   *
   * @return Float - distance in meter
   */
  getDistance: function (point) {
    var distance = 
      Math.round(
        Math.acos(
          Math.sin(toRadian(point.lat)) * 
          Math.sin(toRadian(this.lat)) + 
          Math.cos(toRadian(point.lat)) * 
          Math.cos(toRadian(this.lat)) * 
          Math.cos(toRadian(this.lng) - toRadian(point.lng)) 
        ) * 6378137
      );
    return Math.abs(distance);
  }
    
});



module.exports = Point;