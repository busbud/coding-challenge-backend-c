var haversine = require("haversine-distance");
var similarity = require("string-similarity");

var suggestionRepository = require("../repository/suggestion_repository");
var setup = require("../properties");
var farthest = 1.0;

function cleanUp(suggestion) {
  delete suggestion.similarity;
  delete suggestion.distance;
  delete suggestion.country;
  delete suggestion.admin1;
  delete suggestion.ascii;
  return suggestion;
}

module.exports = {
  search: async function(term, lat, lon) {
    var suggestions = await suggestionRepository.getSuggestionByTerm(
      term,
      lat,
      lon
    );

    scoredSuggestions = suggestions
      .map(function(s) {
        s.name = s.ascii + ", " + s.admin1 + ", " + s.country;

        s.similarity = similarity.compareTwoStrings(term, s.ascii);
        s.distance = 1.0;
        if (lat && lon) {
          var from = ({ latitude, longitude } = s);
          var to = { latitude: lat, longitude: lon };
          s.distance = haversine(from, to);
          farthest = s.distance > farthest ? s.distance : farthest;
        }
        return s;
      })
      .map(function(s) {
        var sw = setup.app.weight.similarity;
        var dw = 1 - sw;
        s.score = s.similarity * sw + dw - (s.distance / farthest) * dw;
        return s;
      })
      .map(s => cleanUp(s))
      .sort((a, b) => b.score - a.score);

    return scoredSuggestions;
  }
};
