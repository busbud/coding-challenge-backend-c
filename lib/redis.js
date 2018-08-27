'use strict';

const config = require('config');
const redisPoolModule  = require('sol-redis-pool');

const log = require('./logger')('redis');

let redisPool = null;

// Redis wrapper so we can catch the query, handle the connection, and forward it to the DB
exports.redisQueryPromise = function redisQueryPromise(...args) {
  return new Promise((resolve, reject) => {
    exports.redisQuery(...args, (err, result) => {
      if(err) return reject(err);
      return resolve(result);
    });
  });
};

exports.redisQuery = function redisQuery(...args) {
  // Get redis connection from the pool
  redisPool.acquire((err, client) => {
    if(err) {
      log.e('Failed to retrieve redis client from the pool');
      process.exit(1);
    }
    // Remove the first argument of the itially called function (the name of the redis method)
    const fn = args.shift();
    // Override the callback
    const cb = args.pop();
    function newCb(...cbArgs) {
      // Release the client into the pool after the call
      redisPool.release(client);
      cb(...cbArgs);
    };
    args.push(newCb);
    // Call the right redis method
    client[fn](...args);
  });
};

exports.getClient = function getClient() {
  const redisSettings = config.db.redis.url ? {url: config.db.redis.url} : config.db.redis;

  if(!config) log.f('Please provide a database config');
  try {
    if(!redisPool || !redisPool.getPoolSize()) {
      const poolSettings = {max: 10, min: 2};
      redisPool = redisPoolModule(redisSettings, poolSettings);
      log.i('Redis client pool created.');
    }
  } catch(e) {
    log.f('Redis client pool failed to initialize.', e);
  }

  return exports.redisQueryPromise;
};
