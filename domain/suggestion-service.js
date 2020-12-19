const repository = require('../repository/suggestion-repo');

module.exports.getSuggestions = async (query) => {

    let suggestions = repository.getSuggestions(query.q, query.latitude, query.longitude);

    return suggestions;
}
