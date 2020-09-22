import suggestionService from './suggestion.service.js';
import errorHandler from '../../error/error.genericHandler.js';

const getSuggestion = errorHandler.wrapAsync(async (req, res) => {
  const { q: searchTerm, latitude, longitude } = req.query;
  const suggestions = await suggestionService.getSuggestions(searchTerm, latitude, longitude);
  return res.send({ suggestions });
});

export default {
  getSuggestion,
};
