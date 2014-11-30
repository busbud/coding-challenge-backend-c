(function () {
  'use strict';

  var Cluster = require('./cluster'),
      logger = require('./helpers/logger'),
      PORT = process.env.PORT || 2345,
      MEMCACHED = process.env.MEMCACHED || null,
      DEBUG_MODE = process.env.DEBUG || false;

  var servers = new Cluster('suggestions',{
    'port':PORT,
    'memcached':MEMCACHED
  });
  servers.initialize();

  if(DEBUG_MODE != false) servers.watchMemory();
})();