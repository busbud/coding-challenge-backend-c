const redis = require('redis');

const client = redis.createClient({
  host: process.env.REDIS_URL ? (process.env.REDIS_HOST || 'localhost') : undefined,
  url: process.env.REDIS_URL || undefined,
});

module.exports = { client };
