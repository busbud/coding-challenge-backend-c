var _ = require('lodash');

var locationParser = require('./location-parser');

// @TODO: could add pagination... not really necessary for an autocomplete api...
var resultLimit = 10;
var data = _.chain([]),
    loaded = false,
    isLoading = false;

function find(query, cb) {
  // to be ready for ElasticSearch or any other prod-proof solution
  cb(
    null, 
    data
      .map(function (location) {
        return location.getSuggestion(query)
      })
      .sortBy(function (suggestion) {
        return -suggestion.score;
      })
      .slice(0, query.limit || 5)
      .filter(function (suggestion) {
        // if below 0.15, then it is really bad... so let's truncate
        return suggestion.score > (query.threeshold || 0.15);
      })
      .map(function (suggestion) {
        return _.extend({score: suggestion.score}, suggestion.location.toJSON());
      })
      .value()
  );
}

function load (dataPath, cb) {
  isLoading = true;
  locationParser.load(dataPath, function (err, locations) {
    if (err) {
      return cb(err);
    }
    loaded = true;
    data = _.chain(locations);
    cb(null);
  });
}

module.exports = {
   _data: data,
   _reset: function () {
    loaded = false;
    isLoading = false;
   },
   connect: function (path, cb) {
    if (!isLoading) {
      return load(path, cb);
    }
    if (loaded) {
      return cb();
    }
    // try again in 50 ms...
    var self = this;
    setTimeout(function () {
      self.connect(path, cb);
    }, 50);
  },
  find: find
};