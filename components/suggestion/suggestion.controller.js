import suggestionService from './suggestion.service.js';

const getSuggestion = async (req, res) => {
  const { q: searchTerm, latitude, longitude } = req.query;
  const suggestions = await suggestionService.getSuggestions(searchTerm, latitude, longitude);

  res.send(suggestions);
};

export default {
  getSuggestion,
};
