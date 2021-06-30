const fp = require('fastify-plugin')
const suggestionsRoute = require('./suggestions')

module.exports = fp((fastify, opts, next) => {
  suggestionsRoute(fastify)
  next()
}, {
  name: 'suggestions-routes'
})
