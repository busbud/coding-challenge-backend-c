// setup the API server specifically for Vercel serverless
// https://busbud.spaans.ca
import Fastify from 'fastify'
import openapiGlue from 'fastify-openapi-glue'

import { Service } from './../service.js'

const Suggestions = new Service()
const specification = {
  openapi: "3.0.0",
  info: {
    title: "Busbud Backend Coding Challenge",
    description: "Autocomplete suggestion API for large cities.",
    version: "0.0.0"
  },
  servers: [
    {
      url: "https://busbud.spaans.ca",
      description: "Demo server for Busbud evaluation."
    }
  ],
  paths: {
    "/suggestions": {
      get: {
        summary: "Returns a list of autocomplete suggestions of names of large cities in Canada and the USA, with a confidence score given for each suggestion.",
        operationId: 'getSuggestions',
        parameters: [
          {
            name: "q",
            "in": "query",
            description: "Partial, or complete, search query of a city name.",
            required: true,
            schema: {
              type: "string"
            },
            examples: {
              partial: {
                value: "Tor",
                summary: "Toronto, maybe?"
              },
              complete: {
                value: "Sault-Ste-Marie",
                summary: "Unambiguous city name."
              }
            }
          },
          {
            name: "latitude",
            "in": "query",
            description: "The client's current or target latitude, as a hint for their searches.",
            required: false,
            schema: {
              type: "number",
              format: "float"
            },
            examples: {
              lat: {
                value: "43.70011"
              }
            },
          },
          {
            name: "longitude",
            "in": "query",
            description: "The client's current or target longitude, as a hint for their searches.",
            required: false,
            schema: {
              type: "number",
              format: "float"
            },
            examples: {
              "long": {
                value: "-79.4163"
              }
            }
          }
        ],
        responses: {
          '200': {
            description: "A list, possibly empty, of suggestions based on the given query parameters.",
            content: {
              "application/json": {
                schema: {
                  type: "object",
                  properties: {
                    suggestions: {
                      type: "array",
                      description: "Suggestion",
                      items: {
                        "$ref": '#/components/schemas/Suggestion'
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  },
  components: {
    schemas: {
      Suggestion: {
        properties: {
          name: {
            type: "string",
            description: "The city name."
          },
          latitude: {
            type: "string",
            description: "The city's latitude."
          },
          longitude: {
            type: "string",
            description: "The city's longitude."
          },
          score: {
            type: "string",
            description: "A confidence score for how accurate the suggestion is based on the input parameters."
          }
        },
        example: {
          name: "London, ON, Canada",
          latitude: "42.98339",
          longitude: "-81.23304",
          score: "0.9"
        }
      }
    }
  }
}

const options = {
  specification,
  service: new Service(),
  prefix: 'v1'
}

const fastify = Fastify({
  logger: true
})

//fastify.register(openapiGlue, options)
fastify.get('/suggestions', Suggestions.getSuggestions)

export default async (req, resp) => {
  await fastify.ready()

  fastify.server.emit('request', req, resp)
}
