const SearchEngineClient = require('./SearchEngineClient')

/**
 * @function
 * @description create a search engine client
 * @param {Object} options
 * @param {string} options.node
 * @param {number} [options.maxRetries=5]
 * @param {number} [options.requestTimeout=60000]
 * @param {Object} [options.auth]
 * @param {string|null} [options.auth.type=null]
 * @param {string} [options.auth.username]
 * @param {string} [options.auth.password]
 * @param {string} [options.auth.apiKey]
 * @param {Logger} logger
 */
module.exports = ({
  nodes,
  maxRetries = 5,
  requestTimeout = 60000,
  auth = {
    type: null
  },
  logger
}) => {
  const searchEngineClient = new SearchEngineClient({
    nodes,
    maxRetries,
    requestTimeout
  })

  searchEngineClient.on('connecting', () => {
    logger.info('Connecting to search engine...')
  })

  searchEngineClient.on('retry', () => {
    logger.info('Connection to search engine failed. Retrying...')
  })

  searchEngineClient.on('connected', () => {
    logger.info('Connected to search engine')
  })

  searchEngineClient.on('closed', () => {
    logger.info('Connection to search engine closed')
  })

  return searchEngineClient
}
