/**
 * Location class
 * 
 * Represent a named location on a map.
 * One property is a geoPoint (lat/lng)
 *
 * Note that for performance reason all attributes are calculated in advance.
 * 
 */

var _ = require('lodash');
var rmDiacritics = require('diacritics').remove;

var GeoPoint = require('./geo-point');
var score = require('./score');
var InvalidParameterError = require('./errors').InvalidParameterError;

function cleanString(str) {
  return rmDiacritics(str.toLowerCase(str));
};

function Location (data) {
  if (!data.name) {
    throw new InvalidParameterError('name property is required');
  }
  this.geoPoint = new GeoPoint(data.latitude, data.longitude);
  _.extend(this, data);
  // calculate displayName once
  this.displayName = this.getDisplayName();
  this.comparableString = cleanString(this.name);
}


_.extend(Location.prototype, {
  getDisplayName: function () {
    return _.compact([this.name, this.state, this.country]).join(', ');
  },

  toJSON: function () {
    return {
      name: this.displayName,
      latitude: this.latitude,
      longitude: this.longitude
    }
  },

  getScore: function (query) {
    var textScore =  score.getText(this.comparableString, cleanString(query.q));
    // do not continue if no text match
    if (!query.geoPoint || textScore == 0) {
      return textScore;
    }
    var distanceScore = score.getDistance(this.geoPoint.getDistance(query.geoPoint));
    return score.getCombined(textScore, distanceScore);
  },

  getSuggestion: function (query) {
    return {
      score: this.getScore(query),
      location: this
    };
  }
});

module.exports = Location;