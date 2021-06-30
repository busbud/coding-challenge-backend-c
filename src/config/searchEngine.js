/**
 * Initialize the search engine configuration.
 * @param {Object} env - The environment variables.
 */
module.exports = (env) => ({
  nodes: env.SEARCH_ENGINE_NODES,
  maxRetries: env.SEARCH_ENGINE_MAX_RETRIES,
  requestTimeout: env.SEARCH_ENGINE_REQUEST_TIMEOUT,
  auth: {
    type: env.SEARCH_ENGINE_AUTH_TYPE || null,
    username: env.SEARCH_ENGINE_AUTH_USERNAME,
    password: env.SEARCH_ENGINE_AUTH_PASSWORD,
    apiKey: env.SEARCH_ENGINE_AUTH_API_KEY
  }
})
