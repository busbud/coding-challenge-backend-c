const winston = require('winston');

/**
 * Creates a winston.Logger instance
 * @param {Object} config Configuration dictionary
 * @returns {winston.Logger} instantiated logger
 */
module.exports = function (config) {
  const logger = winston.createLogger({
    level: config.log.level,
    format: winston.format.json(),
    exitOnError: false,
    transports: [
      new winston.transports.File({ filename: './log/applog', level: 'error' })
    ]
  });

  if (process.env.NODE_ENV !== 'production') {
    logger.add(new winston.transports.Console({
      format: winston.format.simple()
    }));
  }

  logger.stream = {
    write: function (message) {
      logger.info(message);
    }
  };

  return logger;
};