import suggestionService from './suggestion.service.js';

const getSuggestion = (req, res) => {
    const { q: searchTerm, latitude, longitude } = req.params
    const suggestions = suggestionService.getSuggestions(searchTerm, latitude, longitude)

    res.send(suggestions);
}

export default {
    getSuggestion,
}