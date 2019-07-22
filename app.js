// General Requirements
const express       = require('express');
const log4js        = require('log4js');
const morgan        = require('morgan');
// Application Code
const routes        = require('./routes');
const config        = require('./config');
const {
  importData
}                   = require('./lib/loadData');
const app           = express(); // setting up express app
const serverConfig  = config.server;
const logger        = log4js.getLogger();
logger.level        = 'debug';

const DATA_PATH     = `${__dirname}/data/cities_canada-usa.tsv`;
const DATA_DELIM    = '\t';

logger.info(config);

// configure application logging
app.use(morgan(((config.env === 'production') ? 'short' : 'dev')));

// setup routes
app.use(routes);
// importing data
importData(DATA_PATH, DATA_DELIM,function(){
  // prevent conflicting port while developing and testing
  let port = serverConfig.port;
  // App can only start listening once data has been loaded
  app.listen(port, serverConfig.host, () => {
    logger.info(`app running on http://${serverConfig.host}:${serverConfig.port} in ${config.env}`);
    app.emit('appReady');
  });
});


module.exports = app;