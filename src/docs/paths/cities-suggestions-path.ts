export const citiesSuggestionsPath = {
  get: {
    security: [
      {
        apiKeyAuth: [],
      },
    ],
    tags: ['Cities'],
    summary: '',
    description: '',
    parameters: [
      {
        in: 'query',
        name: 'q',
        description: 'Search terms',
        required: true,
        schema: {
          type: 'string',
        },
      },
      {
        in: 'query',
        name: 'latitude',
        description: 'User\'s current latitude',
        schema: {
          type: 'string',
        },
      },
      {
        in: 'query',
        name: 'longitude',
        description: 'User\'s current longitude',
        schema: {
          type: 'string',
        },
      },
    ],
    responses: {
      200: {
        description: 'OK',
        content: {
          'application/json': {
            schema: {
              $ref: '#/schemas/citiesSuggestions',
            },
          },
        },
      },
      404: {
        description: 'Not found',
        content: {
          'application/json': {
            schema: {
              $ref: '#/schemas/citiesSuggestions',
            },
            example: []
          },
        }
      },
      400: {
        $ref: '#/components/badRequest',
      },
      500: {
        $ref: '#/components/serverError',
      },
    },
  },
};
