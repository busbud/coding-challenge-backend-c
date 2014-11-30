(function () {
  'use strict';

  var Cluster = require('./cluster'),
      logger = require('./helpers/logger'),
      PORT = process.env.PORT || 2345,
      DEBUG_MODE = process.env.DEBUG_MODE || 'false';

  var servers = new Cluster('suggestions', PORT);
  servers.initialize();

  if(DEBUG_MODE) servers.watchMemory();

})();