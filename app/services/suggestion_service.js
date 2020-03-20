var suggestionRepository = require('../repository/suggestion_repository')

module.exports = {
    search: async function (term, lat, lon) {
        var suggestions = await suggestionRepository.getSuggestionByTerm(term, lat, lon);
        return suggestions.map(function (s) {
            s.score = 0.0;
            return s;
        })
    }
};