const cacheManager = require('cache-manager')
const redisStore = require('cache-manager-ioredis')

/**
 * @typedef {Object} BuilderCacheManager
 * @function
 * @description Build the cache manager
 * @param {Object} options
 * @param {boolean} [options.isEnabled=true]
 * @param {string} [options.host='localhost']
 * @param {number} [options.port=6379]
 * @param {number} [options.password=null]
 * @param {Object} [options.db=0]
 * @param {string|null} [options.ttl=600]
 * @param {Logger} logger
 * @return {CacheManager}
 */
module.exports = ({
  isEnabled = true,
  host = 'localhost',
  port = 6379,
  password = null,
  db = 0,
  ttl = 600,
  logger
}) => {
  const redisCache = cacheManager.caching({
    store: isEnabled ? redisStore : 'none',
    host,
    port,
    password,
    db,
    ttl
  })

  if (isEnabled) {
    const redisClient = redisCache.store.getClient()

    redisClient.on('error', (error) => {
      logger.error(error)
    })

    redisClient.on('connect', () => {
      logger.info('Connected to redis cache manager')
    })

    redisClient.on('end', () => {
      logger.info('Connection to redis cache manager closed')
    })
  }

  /**
   * @typedef {Object} CacheManager
   */
  return {
    wrap (...args) {
      return redisCache.wrap(...args)
    },
    async close () {
      if (isEnabled) {
        return redisCache.store.getClient().disconnect()
      }
    },
    async healthCheck() {
      if (isEnabled) {
        const redisClient = redisCache.store.getClient()
        await redisClient.ping();
      }
      return {
        status: 'OK'
      }
    }
  }
}
