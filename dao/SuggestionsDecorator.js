const ScoreCalculator = require('../utilties/ScoreCalculator');

module.exports = {
  decorate: (suggestions, search) => {
    let suggestionsResult = suggestions;
    const { latitude, longitude } = search;

    if (latitude && longitude) {
      suggestionsResult = ScoreCalculator.calculateDistanceScore(suggestionsResult, latitude, longitude);
    } else {
      suggestionsResult = ScoreCalculator.calculateTextScore(suggestionsResult, search);
    }

    // Descending order sorting by score
    suggestionsResult.suggestions = suggestionsResult.sort((x, y) => {
      return y.score - x.score;
    });

    return suggestionsResult;
  },
};
