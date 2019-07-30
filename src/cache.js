const redis = require("redis");
let client;
let ready = false;
const { REDIS_URL } = process.env;

/**
 * Create a redis client and make it accessible through exports.redisClient
 */
const createClient = () => {
  client = redis.createClient(REDIS_URL);
  client.on("error", err => {
    console.log(err);
  });
  client.on("ready", () => {
    ready = true;
    console.log("redis is ready");
  });
};

/**
 * @returns { RedisClient }
 */
const redisClient = () => {
  return client;
};

/**
 * @returns {Boolean}
 */
const redisIsReady = () => {
  return ready;
};

module.exports = {
  createClient,
  redisClient,
  redisIsReady
};
