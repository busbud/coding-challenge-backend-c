const logger = require('winston');

logger.level = process.env.LOG_LEVEL || 'debug';

module.exports = logger;
