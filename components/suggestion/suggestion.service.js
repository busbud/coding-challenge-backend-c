import suggestionRepository from './suggestion.repository.js';
import error from '../../error/error.response.js';

const getSuggestions = async (searchTerm, latitude, longitude) => {
  let suggestions = [];
  if (latitude && longitude) {
    suggestions = await suggestionRepository.getSuggestionBySearchTermAndCoordiantes(searchTerm, latitude, longitude);
  } else {
    suggestions = await suggestionRepository.getSuggestionsBySearchTerm(searchTerm);
  }
  if (!suggestions.length) {
    throw error(404, { suggestions });
  }
  return suggestions;
};

export default {
  getSuggestions,
};
