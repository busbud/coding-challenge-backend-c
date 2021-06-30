const signals = ['SIGTERM', 'SIGINT', 'SIGQUIT']

/**
 * @function
 * @description Register graceful shutdown handlers
 * @param {Object} services
 * @param {Object} services.logger - A logger service
 * @return {GracefulShutdown}
 */
function registerShutdown ({ logger }) {
  const handlers = []

  const handleExit = async (exitCode = 0) => {
    try {
      logger.info('Shutting down...')
      for (const handler of handlers) {
        await handler()
      }
      logger.info('Done ! See you later !')
      process.exit(exitCode)
    } catch (error) {
      logger.fatal(error, 'Error during shutdown')
      process.exit(1)
    }
  }

  for (const signal of signals) {
    process.on(signal, async (signal) => {
      logger.info(`Received ${signal} signal`)
      await handleExit()
    })
  }

  process.on('unhandledRejection', async (reason, p) => {
    logger.fatal(reason, 'unhandledRejection occured !')
    await handleExit(1)
  })

  process.on('uncaughtException', async (error) => {
    logger.fatal(error, 'uncaughtException occured !')
    await handleExit(1)
  })

  /**
   * @typedef {Object} GracefulShutdown
   * @property {function} addHandler - Register a custom handler called when the application shutdown
   */
  return {
    addHandler (handler) {
      handlers.push(handler)
    }
  }
}

module.exports = registerShutdown
