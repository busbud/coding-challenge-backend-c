import suggestionService from './suggestion.service.js';
import errorHandler from '../../error/error.genericHandler.js';
import converter from './suggestion.converter.js';

const getSuggestion = errorHandler.wrapAsync(async (req, res) => {
  const { q: searchTerm, latitude, longitude } = req.query;
  const suggestions = await suggestionService.getSuggestions(searchTerm, latitude, longitude);
  return res.send({ suggestions: suggestions.map(converter.fromModel) });
});

export default {
  getSuggestion,
};
