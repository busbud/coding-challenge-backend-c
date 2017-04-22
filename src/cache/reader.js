const redis = require('redis');
const url = require('url');
const FakeCache = require('./fake').default;


let cache = null;

/**
 * Fetches the cache backend through a Promise for all caching usage.
 */
export default function getCache() {
  return new Promise((resolve) => {
    if (cache) {
      resolve(cache);
    } else if (process.env.REDISTOGO_URL !== undefined) {
      const rtg = url.parse(process.env.REDISTOGO_URL);
      cache = redis.createClient(rtg.port, rtg.hostname);
      cache.auth(rtg.auth.split(':')[1]);

      // cache.on('error', (err) => {
      //   console.log(`Redis Cache Error: ${err}`);
      // });

      resolve(cache);
    } else {
      cache = new FakeCache();
      resolve(cache);
    }
  });
}
