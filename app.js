// General Requirements
const express = require('express');
const log4js = require('log4js');
const morgan = require('morgan');
// Application Code
const routes = require('./routes');
const config = require('./config');
const { importData, getData } = require('./lib/importData');
// setting up express app
const app = express();
const serverConfig = config.server;
// setting up logger
const logger = log4js.getLogger();
logger.level = 'debug';
// data file configuration
const { DATA_PATH, DATA_DELIMITER, DATA_ENCODING } = require('./constants');

// configure application logging
app.use(morgan(((config.env === 'production') ? 'short' : 'dev')));
logger.info(config);

// setup routes
app.use(routes);

// importing data
importData(DATA_PATH, DATA_DELIMITER, DATA_ENCODING, function() {
  logger.info(`Imported ${getData().length} cities`);
  app.listen(serverConfig.port, serverConfig.host, () => {
    logger.info(`app running on http://${serverConfig.host}:${serverConfig.port} in ${config.env}`);
    app.emit('appReady');
  });
});

module.exports = app;
