var _        = require('lodash');
var sha1     = require('sha1');
var memcache = require('memcache');

var AN_HOUR = 60 * 60;

function Cache(port, host) {
  this.client = new memcache.Client(port, host);
  this.connected = false;

  this._setupClientHooks();
}

Cache.prototype._setupClientHooks = function() {
  var self = this;

  this.client.on('connect', function() {
    self.connected = true;
    console.log('Connected to memcached!');
  });

  this.client.on('timeout', function() {
    throw new Error([
      'Connecting to memcached timed out with host: "',
      this.host,
      '" and port: "',
      this.port,
      '".'
    ].join());
  });

  this.client.on('error', function(err) {
    throw err;
  });
};

// Takes all search params, concats them, returns a sha1 hash
Cache.prototype._getCacheKey = function(params) {
  var values = _(params).values();

  var key_string = _.reduce(values, function(a, b) {
    return a + b;
  }, '');

  return sha1(key_string);
};

Cache.prototype.connect = function() {
  this.client.connect();
};

Cache.prototype.close = function() {
  this.client.close();
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
