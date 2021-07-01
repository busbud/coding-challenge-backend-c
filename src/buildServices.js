const {
  cacheManager: buildCacheManager,
  logger: buildLogger,
  searchEngine: buildSearchEngine
} = require(
  './services')

/**
 * @function
 * @description Initialize the services
 * @param {Object} config
 * @param {Array} [repositories=[]]
 * @return {services}
 */
module.exports = async (config, repositories = []) => {
  const logger = buildLogger(config.logger)

  logger.info('Initialization of services...')

  const cacheManager = buildCacheManager(
    { ...config.cache, logger })

  const searchEngineClient = buildSearchEngine(
    { ...config.searchEngine, logger })

  searchEngineClient.once('connected', () => {
    for (const { name, repository } of repositories) {
      searchEngineClient.addRepository(name, repository)
    }
  })

  logger.info('Initialization of services done !')

  /**
   * @typedef {Object} services
   * @property {CacheManager} cacheManager
   * @property {Logger} logger
   * @property {SearchEngineClient} searchEngineClient
   */
  return { cacheManager, logger, searchEngineClient }
}
