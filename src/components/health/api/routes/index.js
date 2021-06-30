const fp = require('fastify-plugin')
const healthCheckRoute = require('./healthCheck')

module.exports = fp((fastify, opts, next) => {
  healthCheckRoute(fastify)
  next()
}, {
  name: 'health-routes'
})
