const Fastify = require('fastify')
const fastifyCors = require('fastify-cors')
const fastifyHelmet = require('fastify-helmet')
const metrics = require('fastify-metrics')
const { documentation, errorHandlers } = require('./plugins')

/**
 * @function
 * @description Initialize the application
 * @param {Object} config
 * @param {Object} apiDocs
 * @param {Object} routers
 * @param {Object} services
 * @param {Logger} services.logger - A logger service
 * @param {SearchEngineClient} services.searchEngineClient - A search engine client service
 * @param {Object} services.cacheManager - A cache manager service
 * @return {Fastify}
 */
module.exports = async (
  config,
  apiDocs,
  routers,
  {
    logger,
    searchEngineClient,
    cacheManager
  }) => {
  logger.info('Initialization of application...')

  const fastify = Fastify({ logger })

  fastify.addHook('onClose', (instance, done) => {
    logger.info('Closing the server...')
    done()
  })

  fastify.decorate('cacheManager', cacheManager)
  fastify.decorate('searchEngineClient', searchEngineClient)

  fastify.register(errorHandlers)
  fastify.register(fastifyCors, config.server.cors)
  fastify.register(fastifyHelmet, { contentSecurityPolicy: false })
  fastify.register(documentation, { ...config.app, ...apiDocs })
  for (const { name, routes } of routers) {
    logger.info(`Registering the ${name} routes`)
    fastify.register(routes)
  }
  fastify.register(metrics, { endpoint: '/metrics' })

  logger.info('Initialization of application done !')
  return fastify
}
