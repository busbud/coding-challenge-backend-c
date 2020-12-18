const { makeSuggestion } = require('./suggestion-model');

module.exports.getSuggestions = async (query) => {

    let suggestions = [];

    suggestions.push(makeSuggestion('aaa', 123, 1234.2, 9));

    return suggestions;
}
