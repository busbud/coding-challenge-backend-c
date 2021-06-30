const { version } = require('../../package')

/**
 * Initialize the application configuration.
 * @param {Object} env - The environment variables.
 */
module.exports = (env) => ({
  name: env.APP_NAME,
  description: env.APP_DESCRIPTION,
  version,
  migrationEnabled: env.APP_MIGRATION_ENABLED
})
