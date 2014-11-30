'use strict';

module.exports = (function(){
  var environment = process.env.NODE_ENV || 'development',
        logger = require('./src/helpers/logger'),
        PORT = process.env.PORT || 2345,
        MEMCACHED = null,
        DEBUG_MODE = process.env.DEBUG || false,
        Cluster = require('./src/cluster');

  if (process.env.MEMCACHIER_SERVERS) {
    MEMCACHED = {
      servers: process.env.MEMCACHIER_SERVERS,
      username: process.env.MEMCACHIER_USERNAME,
      password: process.env.MEMCACHIER_PASSWORD
    };
  }


  var cluster = new Cluster('suggestions',{
    'port':PORT,
    'memcached':MEMCACHED
  });
  cluster.initialize();
  if(DEBUG_MODE != false) cluster.watchMemory();
  return cluster;
})();


