'use strict';

module.exports = (function(){
  var environment = process.env.NODE_ENV || 'development',
        logger = require('./helpers/logger'),
        PORT = process.env.PORT || 2345,
        MEMCACHED = process.env.MEMCACHED || null,
        DEBUG_MODE = process.env.DEBUG || false,
        Cluster = require('./cluster');

  var cluster = new Cluster('suggestions',{
    'port':PORT,
    'memcached':MEMCACHED
  });
  cluster.initialize();
  if(DEBUG_MODE != false) cluster.watchMemory();
  return cluster;
})();


