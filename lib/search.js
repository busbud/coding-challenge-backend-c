var _           = require('lodash');
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

  clean_params = _.pick(params, ['q', 'latitude', 'longitude']);
  clean_params = _.defaults(clean_params, {
    q: '',
    latitude: null,
    longitude: null
  });

  return clean_params;
}

Search.prototype.add = function(items) {
  this.search_index.add(items);
};

Search.prototype.search = function(params) {
  // Step 1: Sanitize params
  var params = this._sanitizeSearchParams(params);

  // Step 2: Check the cache
  if (this.cache) {
    var cached_results = this.cache.get(params);
    if (cached_results) {
      return cached_results;
    }
  }

  // Step 3: Make the search
  var results = this.search_index.get(params.q);

  // Step 4: Score the search results
  // TODO: Comming up
  var scored_results = this.scorer.score(results, params);

  // Step 5: Remove unnecessary fields from city objects
  var cleaned_results = _.map(scored_results, function(obj) {
    return obj.toObject()
  });

  // Step 5: Cache results
  if (this.cache) {
    this.cache.set(params, cleaned_results);
  }

  // Step 6: We are done
  return cleaned_results;
}

module.exports = Search;
