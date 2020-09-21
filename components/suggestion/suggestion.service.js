import suggestionRepository from './suggestion.repository.js';

const getSuggestions = async (searchTerm, latitude, longitude) => {
  if (latitude) {
    return suggestionRepository.getSuggestionBySearchTermAndCoordiantes(searchTerm, latitude, longitude);
  }
  return suggestionRepository.getSuggestionsBySearchTerm(searchTerm);
};

export default {
  getSuggestions,
};
