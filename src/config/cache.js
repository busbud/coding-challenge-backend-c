/**
 * Initialize the cache configuration.
 * @param {Object} env - The environment variables.
 */
module.exports = (env) => ({
  isEnabled: env.CACHE_IS_ENABLED,
  host: env.CACHE_HOST,
  port: env.CACHE_PORT,
  password: env.CACHE_PASSWORD || null,
  db: env.CACHE_DB_INDEX,
  ttl: env.CACHE_TTL
})
