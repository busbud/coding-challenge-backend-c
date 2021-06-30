const controllers = require('../controllers')

/**
 * @function
 * @description Initialize the health routes
 * @param {FastifyInstance} fastify
 */
module.exports = (fastify) => {
  fastify.get('/health', {
    schema: {
      description: 'Get the application health',
      tags: ['health'],
      summary: 'Get the application health',
      response: {
        200: {
          description: 'Successful response',
          type: 'object',
          properties: {
            searchEngine: {
              type: 'object',
              description: 'The search engine status',
              properties: {
                clusterName: {
                  type: 'string',
                  description: 'The name of the cluster'
                },
                status: {
                  type: 'string',
                  description: 'The status of the cluster',
                  enum: ['green', 'yellow', 'red']
                }
              }
            },
            cacheManager: {
              type: 'object',
              description: 'The search engine status',
              properties: {
                status: {
                  type: 'string',
                  description: 'The status of the cache manager',
                  enum: ['OK']
                }
              }
            }
          }
        }
      }
    }
  }, controllers.healthCheck)
}
