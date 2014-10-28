var _     = require('lodash');
var sha1  = require('sha1');
var memjs = require('memjs');

var AN_HOUR = 60 * 60;

function Cache(port, host) {
  this.client = memjs.Client.create();
}

// Takes all search params, concats them, returns a sha1 hash
Cache.prototype._getCacheKey = function(params) {
  var values = _(params).values();

  var key_string = _.reduce(values, function(a, b) {
    return a + b;
  }, '');

  return sha1(key_string);
};

Cache.prototype.get = function(params, callback) {
  var key = this._getCacheKey(params);

  this.client.get(key, function(err, result) {
    callback(err, JSON.parse(result));
  });
};

Cache.prototype.set = function(params, results, callback) {
  var key = this._getCacheKey(params);
  var data = JSON.stringify(results);

  this.client.set(key, data, callback, AN_HOUR);
};

module.exports = Cache;
