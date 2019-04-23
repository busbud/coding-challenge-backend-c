const geolib = require('geolib');

const addDistanceToSuggestions = (suggestions, lat, long) => {
  if (lat != null && long != null) {
    const user_Coord = {
      latitude: lat,
      longitude: long
    };
    suggestions.forEach(city_data => {
      const city_Coord = {
        latitude: city_data.latitude,
        longitude: city_data.longitude
      };
      city_data.distanceInKM = geolib.getDistance(user_Coord, city_Coord) / 1000; // distance in kilometers
    });
  }
};

const scoreSuggestions = suggestions => {
  suggestions.forEach(c => { c.score = score(c.distanceInKM, c.population); });
  sortByScore(suggestions);
  normaliseSuggestionScores(suggestions);
};

const score = (distance, population) => {
  let distance_score = distance ? Math.pow(distance, -10 / 3) : 1;
  let population_score = Math.pow(population, 6);
  return distance_score * population_score;
};

const sortByScore = suggestions => suggestions.sort((a, b) => b.score - a.score);

const normaliseSuggestionScores = suggestions => {
  const max_score = suggestions.length >= 1 ? Math.log(suggestions[0].score) : null;
  const min_score = suggestions.length >= 1 ? Math.log(suggestions.slice(-1)[0].score) : null;

  // if min and max are even (happens when only 1 result, or possibly edge cases),
  // use max to get a score of 1 instead of dividing by 0
  const score_diff = (max_score - min_score) || max_score;

  suggestions.forEach(city_data => {
    city_data.score = Math.log(city_data.score); // take the log, to avoid too much variance in the scores

    // normalise the score
    city_data.score -= min_score; // subtract by minimum score to get same baseline
    city_data.score /= score_diff; // divide by difference between min and max

    city_data.score = Math.round(city_data.score * 100) / 100; // round and truncate to 2 decimals
  });
};

module.exports = {};
module.exports.addDistanceToSuggestions = addDistanceToSuggestions;
module.exports.scoreSuggestions = scoreSuggestions;
module.exports.score = score;
module.exports.sortByScore = sortByScore;
module.exports.normaliseSuggestionScores = normaliseSuggestionScores;
