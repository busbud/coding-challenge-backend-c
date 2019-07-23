// General Libraries
const log4js = require('log4js');
const redis = require("redis");
// Application Code
const config = require('../config');

const logger = log4js.getLogger();
logger.level = 'debug';


const client = redis.createClient(config.redis.url, {
  prefix: `${config.redis.namespace}:${config.env}:`
});

client.on('connect', function() {
  logger.info(`Redis - connected`);
});


client.on('ready', function() {
  logger.info(`Redis - ready`);
});

client.on('error', function(message) {
  logger.error(`[Redis Error] - ${message}`);
});

module.exports = client;
