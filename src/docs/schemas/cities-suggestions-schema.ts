export const citiesSuggestionsSchema = {
  type: 'array',
  items: {
    $ref: '#/schemas/citySuggestion',
  },
};
