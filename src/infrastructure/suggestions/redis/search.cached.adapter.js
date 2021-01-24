const crypto = require('crypto');
const client = require('./client');
const { expirationTime } = require('../../../../config').redis;
const { cacheSearchAdapterWhenMissing } = require('../../../../config');
// eslint-disable-next-line import/no-dynamic-require
const adapter = require(cacheSearchAdapterWhenMissing);

const computeRedisKey = (q, latitude, longitude) => `suggestion_${crypto.createHash('md5')
  .update(`q_${q}_l_${latitude}_l_${longitude}`).digest('hex')}`;

const persistCache = (key, suggestion) => new Promise((resolve) => {
  const value = JSON.stringify(suggestion);
  // eslint-disable-next-line no-unused-vars
  client.set(key, value, 'EX', expirationTime, 'NX', (err, reply) => {
    if (err !== null) {
      console.log(err);
    }
    resolve();
  });
});

const fulltextSearch = (q, latitude, longitude) => new Promise((resolve, reject) => {
  const key = computeRedisKey(q, latitude, longitude);
  client.get(key, (err, reply) => {
    if (reply !== null) {
      return resolve(JSON.parse(reply));
    }
    if (err) {
      console.log(err);
    }

    return adapter.fulltextSearch(q, latitude, longitude).then((suggestion) => {
      console.log('hit origin');
      persistCache(key, suggestion).then(resolve(suggestion));
    })
      .catch((reason) => reject(reason));
  });
});

module.exports.fulltextSearch = fulltextSearch;
