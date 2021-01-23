const md5 = require('crypto');
const redis = require('redis');
const adapter = require('../infrastructure/suggestions/search.adapter');

const redisClient = redis.createClient();

module.exports.search = async (q, latitude, longitude) => new Promise((resolve, reject) => {
  const hash = md5.createHash('md5')
    .update(`q_${q}_l_${latitude}_l_${longitude}`).digest('hex');
  const key = `suggestion_${hash}`;
  redisClient.get(key, (err, reply) => {
    if (reply !== null) {
      console.log('hit redis');
      resolve(JSON.parse(reply));
      return;
    }

    adapter.fulltextSearch(q, latitude, longitude).then((result) => {
      console.log('hit elastic');
      redisClient.set(key, JSON.stringify(result));
      resolve(result);
    }).catch((reason) => reject(reason));
  });
});
