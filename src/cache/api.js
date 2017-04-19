const hashify = require('../utils/hash').default;
const getCache = require('./reader').default;


/**
 * Fetches a cached API response of suggestions for the query identified
 * by the three query parameters: the city name and the person's latitude and longitude
 */
function getSuggestionsFromCache(nameQuery, lat, lon) {
  return new Promise((resolve) => {
    const hashKey = hashify(nameQuery, lat, lon);

    getCache().then((cache) => {
      cache.get(hashKey, (err, reply) => {
        let suggestions = reply;
        if (reply !== null) {
          suggestions = JSON.parse(reply);
        }
        resolve(suggestions);
      });
    });
  });
}

/**
 * Puts an API response of suggestions into the cache for future retrieval
 */
function setSuggestionsInCache(suggestions, nameQuery, lat, lon) {
  const hashKey = hashify(nameQuery, lat, lon);

  getCache().then((cache) => {
    cache.set(hashKey, JSON.stringify(suggestions));
  });
}

export { getSuggestionsFromCache, setSuggestionsInCache };
