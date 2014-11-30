var hash = require('object-hash'),
    memjs = require('memjs'),
    logger = require('../helpers/logger');

var AN_HOUR = 60 * 60;

module.exports = (function() {
  'use strict';
  function Cache(servers, ttl, callback) {
    var self = this;
    this.log = logger('cache');
    this.isConnected = false; 
    this.ttl = ttl || 60 * 60; //default: One Hour
    callback = callback || function(){};

    if(typeof(ttl) === 'function') {
      callback = ttl;
      this.ttl = 60 * 60;
    }

    this.client = memjs.Client.create(servers.servers, {
                                                username:servers.username,
                                                password:servers.password,
                                                failoverTime: 60,
                                                retries: 2,
                                                expires: 0 });

    this.client.stats(function(err, server, stats){
       if(err) self.log(err);
       console.log(stats);
       if(stats != null ) self.isConnected = true;
       callback(self.isConnected);
    });
  }  

  Cache.prototype.get = function(params, callback) {
    var key = hash(params);
    var self = this;
    this.client.get(key, function(err, result) {
      if(err) self.isConnected = false;
      if(result) result = JSON.parse(result);

      callback(err, result);
    });
  };

  Cache.prototype.set = function(params, results, callback) {
    var key = hash(params);
    var data = JSON.stringify(results);
    this.client.set(key, data, callback, this.ttl);
  };

  return Cache;

})();