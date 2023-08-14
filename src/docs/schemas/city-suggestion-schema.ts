export const citySuggestionSchema = {
  type: 'object',
  properties: {
    name: {
      type: 'string',
    },
    latitude: {
      type: 'number',
    },
    longitude: {
      type: 'number',
    },
    score: {
      type: 'number',
    },
  },
  required: ['name', 'latitude', 'longitude', 'score'],
};
