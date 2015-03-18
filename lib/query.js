/**
 * Query class
 *
 * Represent a query that user user can do: a query term and optional lat/lng params.
 */
var _ = require('lodash');

var GeoPoint = require('./geo-point');
var InvalidParameterError = require('./errors').InvalidParameterError;

function Query(data) {

  if (!data.q) {
    throw new InvalidParameterError('q parameter is required');
  }
    if (!_.isString(data.q)) {
    throw new InvalidParameterError('q parameter should be a string');
  }
  this.q = data.q;
  if (data.latitude || data.longitude) {
    this.geoPoint = new GeoPoint(data.latitude, data.longitude);
  }
}

module.exports = Query;