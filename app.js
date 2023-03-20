import Fastify from 'fastify'
import openapiGlue from 'fastify-openapi-glue'
import Swagger from '@fastify/swagger'
import SwaggerUI from '@fastify/swagger-ui'

import { Service } from './service.js'

const Suggestions = new Service()
const options = {
  specification: './spec/cities.yaml',
  service: Suggestions,
  prefix: 'v1'
}

const fastify = Fastify({
  logger: true
})

//fastify.register(openapiGlue, options)
fastify.register(Swagger, {
  mode: 'static',
  specification: {
    path: './spec/cities.yaml'
  }
})
fastify.register(SwaggerUI, {
  routePrefix: '/documentation',
  uiConfig: {
    docExpansion: 'full',
    deepLinking: false
  }
})

fastify.get('/suggestions', Suggestions.getSuggestions)

fastify.listen({ host: '0.0.0.0', port: 3000 }, (err, address) => {
  if (err) {
    fastify.log.error(`Problem starting up: ${err}`)
    process.exit(1)
  }
  if (process.env.NODE_ENV === 'development') {
    fastify.log.info(`Server up at ${address}`)
  }

})
