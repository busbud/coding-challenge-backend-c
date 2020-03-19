var suggestionRepository = require('../repository/suggestion_repository')

module.exports = {
    search: async function(term, lat, lon) {
        return await suggestionRepository.getSuggestionByTerm(term, lat, lon);
    }
};