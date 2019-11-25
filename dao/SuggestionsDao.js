// Repository
const SuggestionsRepository = require('./repositories/SuggestionsRepository');
const SuggestionsDecorator = require('./SuggestionsDecorator');

module.exports = {
  getSuggestions: async (ctx, search) => {
    const rawSuggestions = await SuggestionsRepository.getSuggestions(search);

    const { suggestions } = rawSuggestions;
    if (Array.isArray(suggestions)) {
      if (ctx) { ctx.status = 404; }
      return rawSuggestions;
    }

    const decoratedSuggestions = SuggestionsDecorator.decorate(suggestions, search);

    return { suggestions: decoratedSuggestions };
  },
};
