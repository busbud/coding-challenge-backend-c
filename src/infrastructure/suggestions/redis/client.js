const redis = require('redis');
const redisConfig = require('../../../../config').redis;

const client = redis.createClient(redisConfig);

module.exports = client;
