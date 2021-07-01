const controllers = require('../controllers')

/**
 * @function
 * @description Initialize the suggestions routes
 * @param {FastifyInstance} fastify
 */
module.exports = (fastify) => {
  fastify.get('/suggestions', {
    schema: {
      description: 'Get location suggestions for a given query and location',
      tags: ['suggestions'],
      response: {
        200: {
          description: 'Successful response',
          type: 'object',
          properties: {
            suggestions: {
              type: 'array',
              items: {
                type: 'object',
                properties: {
                  name: {
                    type: 'string',
                    description: 'The name of location suggestion'
                  },
                  latitude: {
                    type: 'number',
                    description: 'The latitude of location suggestion'
                  },
                  longitude: {
                    type: 'number',
                    description: 'The longitude of location suggestion'
                  },
                  score: {
                    type: 'number',
                    description: 'The score indicating the confidence in the suggestion'
                  }
                }
              }
            }
          }
        }
      },
      query: {
        type: 'object',
        properties: {
          q: {
            type: 'string',
            description: 'The partial (or complete) search term'
          },
          latitude: { type: 'number' },
          longitude: { type: 'number' },
          radius: {
            type: 'string'
          },
          limit: {
            type: 'number'
          }
        },
        required: ['q']
      }
    }
  }, controllers.suggestions)
}
