const config = require('./config')
const buildServices = require('./buildServices')
const buildApp = require('./buildApp')
const initGracefulShutdown = require('./gracefulShutdown')
const initComponents = require('./components')
const { loadComponents } = require('./helpers')
const applyMigrations = require('./migrations')

module.exports = async () => {
  const components = initComponents()
  const { apiDocs, routers, repositories } = loadComponents(components)

  const { cacheManager, logger, searchEngineClient } = await buildServices(
    config,
    repositories)

  const app = await buildApp(config, apiDocs, routers, {
    cacheManager,
    logger,
    searchEngineClient
  })

  const gracefulShutdown = initGracefulShutdown({ logger })

  gracefulShutdown.addHandler(async () => {
    return Promise.all([
      searchEngineClient.close(),
      cacheManager.close()
    ])
  })

  gracefulShutdown.addHandler(async () => {
    return app.close()
  })

  await searchEngineClient.connect()

  if (config.app.migrationEnabled) {
    logger.info('Start migrations...')
    await applyMigrations(searchEngineClient)
    logger.info('Migration done !')
  }

  await app.listen(config.server.port, config.server.host)

  return app
}
