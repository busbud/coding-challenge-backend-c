const fp = require('fastify-plugin')
const {
  ValidationError,
  NotFoundError,
  InternalServerError
} = require('../errors')

/**
 * @function
 * @description Catch and format errors
 * @param {FastifyInstance} fastify
 * @param {any} opts
 * @param {function} next
 */
function errorHandlers (fastify, opts, next) {
  fastify.setSchemaErrorFormatter((errors, dataVar) => {
    return new ValidationError('Validation failed', {
      info: {
        scope: dataVar,
        errors
      }
    })
  })

  fastify.setNotFoundHandler((request) => {
    const { method, url } = request
    throw new NotFoundError(`Route ${method}:${url} not found`)
  })

  fastify.setErrorHandler(function (error, request, reply) {
    if (!error.isHTTPError) {
      console.log(error)
      error = new InternalServerError(error.message, { cause: error })
    }

    request.log.error(error, 'HTTP error')

    reply.status(error.statusCode).send({
      ...error.payload(),
      requestId: request.id
    })
  })

  next()
}

module.exports = fp(errorHandlers, {
  name: 'error-handlers'
})
