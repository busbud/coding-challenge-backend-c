var hash = require('object-hash'),
    Memcached = require('memcached'),
    logger = require('../helpers/logger');

var AN_HOUR = 60 * 60;

module.exports = (function() {
  'use strict';
  function Cache(address, ttl, callback) {
    var self = this;
    this.log = logger('cache');
    this.isConnected = false; 
    this.ttl = ttl || 60 * 60; //default: One Hour
    callback = callback || function(){};

    if(typeof(ttl) === 'function') {
      callback = ttl;
      this.ttl = 60 * 60;
    }

    this.client = new Memcached(address, {
                                            poolSize: 10            // maximal parallel connections
                                          , retries: 1              // Connection pool retries to pull connection from pool
                                          , minTimeout: 1000        // Connection pool retry min delay before retrying
                                          , maxTimeout: 1000       // Connection pool retry max delay before retrying
                                          , reconnect: 1000    // if dead, attempt reconnect each xx ms
                                          , timeout: 1000           // after x ms the server should send a timeout if we can't connect
                                          , failures: 5             // Number of times a server can have an issue before marked dead
                                          , failuresTimeout: 1000   // Time after which `failures` will be reset to original value, since last failure
                                          , retry: 1000            // When a server has an error, wait this amount of time before retrying
                                          , idle: 1000              // Remove connection from pool when no I/O after `idle` ms
                                          , remove: true           // remove server if dead if false, we will attempt to reconnect
                                          , debug: false            // Output the commands and responses
                                        });
    this.client.connect(address, function( err, conn ){
      if( err ) {
        self.log(err);
      } else {
        self.log('MemCached is ready....');
        self.isConnected = true;
      }
      callback(self.isConnected);
    });

    this.client.on('issue', function( details ){ 
      self.log('Issue with server %s', details.server);
      //TODO: send alert to sysOps
      self.isConnected = false;
    });
    this.client.on('failure', function( details ){ 
      self.log('Server %s went down due to: %s', details.server, details.messages.join( '' ));
      self.isConnected = false;
    });
    this.client.on('reconnecting', function( details ){ 
      self.log( "Total downtime caused by server %s  : %d ms", details.server, details.totalDownTime);
      self.isConnected = false;
    });
    this.client.on('reconnected', function( details ){ 
      self.log("Server %s reconnected by server %s", details.server);
      self.isConnected = true;
    });
  }  

  Cache.prototype.get = function(params, callback) {
    var key = hash(params);

    this.client.get(key, function(err, result) {
      if(result) result = JSON.parse(result);
      callback(err, result);
    });
  };

  Cache.prototype.set = function(params, results, callback) {
    var key = hash(params);
    var data = JSON.stringify(results);

    this.client.set(key, data, this.ttl, callback);
  };

  return Cache;

})();