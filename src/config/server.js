/**
 * Initialize server the configuration.
 * @param {Object} env - The environment variables.
 */
module.exports = (env) => ({
  host: env.SERVER_HOST || '127.0.0.1',
  port: env.SERVER_PORT || 3000,
  cors: {
    origin: env.SERVER_CORS_ORIGIN || true,
    allowedHeaders: env.SERVER_CORS_ALLOWED_HEADERS,
    credentials: env.SERVER_CORS_CREDENTIALS || false
  }
})
