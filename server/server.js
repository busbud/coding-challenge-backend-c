const helmet = require('helmet');
const bodyParser = require('body-parser');
const morgan = require('morgan');
const express = require('express');

/**
 * SERVER MODULE
 * Takes care of initializing the server and it's dependencies
 * @param {Object} config configuration dictionary
 * @param {Winston.logger} logger Logger instance
 * @param {elasticsearch.Client} client ES client
 * @param {Module} db db/index.js module
 */
module.exports.start = async function (config, logger, client, db) {
  const app = express();
  const controllers = require('../controllers')(client, db, config);
  const middleware = require('../middleware');
  const router = require('../routes');

  // setup middleware
  app.use(helmet());
  app.use(bodyParser.json());
  app.use(morgan('combined', {
    stream: logger.stream
  }));
  // custom middleware
  middleware.init(app);

  // router
  app.use('/', router(controllers, logger));

  // start server
  const server = app.listen(config.server.port, () => {
    logger.debug(`server started on port ${config.server.port}`);
  });

  return server;
};