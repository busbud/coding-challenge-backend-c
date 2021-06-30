const fp = require('fastify-plugin')
const swagger = require('fastify-swagger')

/**
 * @function
 * @description Add swagger documentation
 * @param {FastifyInstance} fastify
 * @param {any} opts
 * @param {function} next
 */
function documentation (fastify, opts, next) {
  const { version, name, description } = opts
  fastify.register(swagger, {
    routePrefix: '/documentation',
    openapi: {
      info: {
        title: name,
        description,
        version
      }
    },
    uiConfig: {
      docExpansion: 'full',
      deepLinking: false
    },
    exposeRoute: true
  })
  next()
}

module.exports = fp(documentation, {
  name: 'documentation'
})
