const scoreString = require('string-score');

module.exports = {
  calculateDistanceScore: (suggestions, latitude, longitude) => {
    const suggestionsResult = suggestions.suggestions;
    for (let i = 0; i < suggestionsResult.length; i += 1) {
      let score = 0;
      const lat = Math.abs(suggestionsResult[i].latitude - latitude);
      const long = Math.abs(suggestionsResult[i].longitude - longitude);

      score = 10 - (lat + long) / 2;
      score = score > 0 ? Math.round(score) / 10 : 0;

      suggestionsResult[i].score = score;
    }
    return suggestionsResult;
  },

  calculateTextScore: (suggestions, search) => {
    const { q } = search;
    const suggestionsResult = suggestions.suggestions;

    for (let i = 0; i < suggestionsResult.length; i += 1) {
      let score = 0;
      score = scoreString(suggestionsResult[i].name.split(',')[0], q);
      suggestionsResult[i].score = parseFloat(score).toFixed(1);
    }

    return suggestionsResult;
  },
};
