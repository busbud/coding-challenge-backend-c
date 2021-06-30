/**
 * Initialize the logger configuration.
 * @param {Object} env - The environment variables.
 */
module.exports = (env) => ({
  transports: {
    rotatingFile: {
      enabled: env.LOGGER_TRANSPORT_ROTATING_FILE_ENABLED || false,
      filename: env.LOGGER_TRANSPORT_ROTATING_FILE_FILE_NAME,
      size: env.LOGGER_TRANSPORT_ROTATING_FILE_SIZE,
      interval: env.LOGGER_TRANSPORT_ROTATING_FILE_INTERVAL,
      compress: env.LOGGER_TRANSPORT_ROTATING_FILE_COMPRESS
    },
    stdout: {
      enabled: env.LOGGER_TRANSPORT_STDOUT_ENABLED,
      pretty: env.LOGGER_TRANSPORT_STDOUT_PRETTY
    }
  }
})
