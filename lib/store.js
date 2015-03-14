var _ = require('lodash');

var locationParser = require('./location-parser');

// @TODO: could add pagination... not really necessary for an autocomplete api...
var resultLimit = 10;
var data = _.chain([]);

function find(query, cb) {
  // to be ready for ElasticSearch or any other prod-proof solution
  cb(
    null, 
    data
      .map(function (location) {
        return location.getSuggestion(query)
      })
      .sortBy(function(suggestion) {
          return -suggestion.score;
      })
      .slice(0, 3)
      .map(function (suggestion) {
        return _.extend({score: suggestion.score}, suggestion.location.toJSON());
      })
      .value()
  );
}



module.exports = {
  data: data,
  find: find,
  load: function (dataPath, cb) {
    locationParser.load(dataPath, function (err, locations) {
      if (err) {
        return cb(err);
      }
      data = _.chain(locations);
      cb(null);
    });
  }
};