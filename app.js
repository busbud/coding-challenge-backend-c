// Basic Requirements
const express = require('express');
const log4js = require('log4js');
const morgan = require('morgan');

// App specifc Requirements
const routes = require('./routes');
const config = require('./config');


const app = express(); // setting up express app
const serverConfig = config.server;


// configure application logging
app.use(morgan(((config.env === 'production') ? 'short': 'dev')));
const logger = log4js.getLogger();
logger.level = 'debug';

// setup routes
app.use(routes);

app.listen(serverConfig.port, serverConfig.host, () => {
  logger.info(`app running on http://${serverConfig.host}:${serverConfig.port} in ${config.env}`);
});

