'use strict';

const config = require('config');
const redisPoolModule  = require('sol-redis-pool');

const log = require('./logger')('redis');

let redisPool = null;

function generateRedisClient() {
  const redisSettings = config.db.redis;
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

  // Redis wrapper so we can catch the query, handle the connection, and forward it to the DB
  function redisQueryPromise(...args) {
    return new Promise((resolve, reject) => {
      redisQuery(...args, (err, result) => {
        if(err) return reject(err);
        return resolve(result);
      });
    });
  }

  function redisQuery(...args) {
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
  return redisQueryPromise;
};

module.exports = generateRedisClient();
