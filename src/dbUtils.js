const isConnectedToMongo = require("./mongo").isConnectedToMongo;
const redisIsReady = require("./cache").redisIsReady;

/**
 * Simple sleep method.
 *
 * @param { Number } duration
 * @returns { Promise }
 */
const wait = duration => {
  return new Promise(resolve => {
    setTimeout(resolve, duration);
  });
};

/**
 * Resolve once mongo and redis are ready to handle request. Used in
 * test to prevent the test from runnning before the app is ready.
 *
 * @param { Number } duration
 * @param { Number } maxAttempts
 * @returns { Promise }
 */
const waitForApp = (duration, maxAttempts) => {
  return new Promise(async (resolve, reject) => {
    for (let i = 0; i < maxAttempts; i++) {
      if (isConnectedToMongo() && redisIsReady()) {
        resolve();
      }
      await wait(duration);
    }
    reject(
      new Error(
        "Cannot connect to database and redis after " + maxAttempts + " attemps"
      )
    );
  });
};

module.exports = {
  waitForApp
};
