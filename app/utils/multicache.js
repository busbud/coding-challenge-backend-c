var cacheManager = require('cache-manager');
var mongoCache = cacheManager.caching({store: require('cache-manager-mongodb'), uri: require('../../config/config').db.cacheUri, options: {/*collection: 'cache'*/}, ttl: 600});
var memoryCache = cacheManager.caching({store: 'memory', max: 100, ttl: 60});
module.exports = cacheManager.multiCaching([memoryCache, mongoCache]);
