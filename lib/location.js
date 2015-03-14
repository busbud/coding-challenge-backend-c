var _ = require('lodash');
var rmDiacritics = require('diacritics').remove;

var GeoPoint = require('./geo-point');
var score = require('./score');
var InvalidParameterError = require('./errors').InvalidParameterError;

function cleanString(str) {
  return rmDiacritics(str.toLowerCase(str));
};

function Location (data) {
  // @TODO: validate received properties...
  if (!data.name) {
    throw new InvalidParameterError('name property is required');
  }
  this.geoPoint = new GeoPoint(data.latitude, data.longitude);
  _.extend(this, data);
  this.displayName = this.getDisplayName();
  this.comparableString = cleanString(this.name);
}


_.extend(Location.prototype, {
  // @TODO: change display name when name, state.
  // this requires a `set` method (à la `Backbone`) or 'Object.defineProperty'...
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
    // @TODO: add a function that returns a distanceScore,
    // (ie. 1 if geoPoint are equals (distance = 0 and 0 if greater than 200km (linear for now...)
    // then assign weight to the textScore and distanceScore
    return textScore;
  },

  getSuggestion: function (query) {
    return {
      score: this.getScore(query),
      location: this
    };
  }
});

module.exports = Location;