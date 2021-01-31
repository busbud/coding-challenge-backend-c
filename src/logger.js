const logger = require('winston');

logger.level = process.env.LOG_LEVEL || 'info';

module.exports = logger;
