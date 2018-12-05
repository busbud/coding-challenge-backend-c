'use strict';

const logger = require('./logger');

/**
 * Error Handler
 * @param error
 */
module.exports.onError = function(error) {
  if (error.syscall !== 'listen') {
    throw error;
  }

  const bind = typeof process.env.PORT === 'string' ? 'Pipe ' + process.env.PORT : 'Port ' + process.env.PORT;
  // handle specific listen errors with friendly messages
  switch (error.code) {
    case 'EACCES':
      logger.error(bind + ' requires elevated privileges');
      process.exit(1);
      break;
    case 'EADDRINUSE':
      logger.error(bind + ' is already in use');
      process.exit(1);
      break;
    default:
      logger.error('UncaughtException', error);
      throw error;
  }
};
