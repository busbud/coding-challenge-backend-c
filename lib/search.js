var _           = require('lodash');
var async       = require('async');
var SearchIndex = require('../vendor/search_index');
var Scorer      = require('./scorer');

// Tokenizers
function cityTokenizer(obj) {
  str = obj.ascii.toLowerCase();
  return str ? str.split(/\W+/) : [];
}
function queryTokenizer(obj) {
  str = obj.toString().toLowerCase();
  return str ? str.split(/\W+/) : [];
}


// Search class
function Search() {
  this.cache = null;
  this.scorer = new Scorer();
  this.search_index = new SearchIndex({
    datumTokenizer: cityTokenizer,
    queryTokenizer: queryTokenizer
  });
}

Search.prototype._sanitizeSearchParams = function(params) {
  var clean_params;

  clean_params = _.pick(params, ['q', 'latitude', 'longitude', 'limit']);
  clean_params = _.defaults(clean_params, {
    q: '',
    latitude: null,
    longitude: null,
    limit: 10
  });

  return clean_params;
}

Search.prototype.add = function(items) {
  this.search_index.add(items);
};

Search.prototype.search = function(params, done) {
  var self = this;

  async.waterfall([

    // Step 1: Sanitize params
    function(callback) {
      var sanitized_params = self._sanitizeSearchParams(params);
      callback(null, sanitized_params);
    },


    // Step 2: Check the cache
    function(params, callback) {
      if (self.cache) {

        // Let's check in the cache
        self.cache.get(params, function(err, cached_results) {
          if (err) return callback(err);

          if (cached_results) {

            // Cool, there was something, pass it down
            callback(null, params, cached_results);

          } else {

            // Dammit nothing there
            callback(null, params, null);

          }
        });

      } else {

        // There is no cache set up
        callback(null, params, null);

      }
    },

    // Step 3: Make the search (only if no cached ones present)
    function(params, cached_results, callback) {
      var results = null;

      if (!cached_results) {
        results = self.search_index.get(params.q);
      }

      callback(null, params, cached_results, results);
    },


    // Step 4: Score the search results (only if no cached ones present)
    function(params, cached_results, results, callback) {
      var scored_results = null;

      if (!cached_results) {
        scored_results = self.scorer.score(results, params);
      }

      callback(null, params, cached_results, scored_results);
    },

    // Step 5: Remove unnecessary fields from city objects
    function(params, cached_results, scored_results, callback) {
      var cleaned_results = null;

      if (!cached_results) {
        cleaned_results = _.map(scored_results, function(obj) {
          return obj.toObject()
        });
      }

      callback(null, params, cached_results, cleaned_results);
    },

    // Step 6: Limit the results
    function(params, cached_results, cleaned_results, callback) {
      var limited_results = cleaned_results;

      // If we have a limti and we are serving uncached results
      // limit the results using slice
      if (params.limit && cleaned_results) {
        var limit = Math.min(params.limit, limited_results.length);
        limited_results = limited_results.slice(0, limit);
      }

      callback(null, params, cached_results, limited_results);
    },

    // Step 7: Cache results
    function(params, cached_results, limited_results, callback) {
      if (cached_results) {

        callback(null, cached_results);

      } else if (self.cache) {

        // We got a cache, store results for next request and go on
        self.cache.set(params, limited_results, function(err, result) {
          if (err) return callback(err);
          callback(null, limited_results);
        });

      } else {

        // No cached results and no cache to put them in
        callback(null, limited_results);

      }
    }
  ], function(err, results) {
    if (err) throw err;

    done(results);
  });
}

module.exports = Search;
