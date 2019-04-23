const geolib = require('geolib');

const addDistanceToSuggestions = (suggestions, lat, long) => {
  if (lat != null && long != null) {
    const userCoord = {
      latitude: lat,
      longitude: long
    };
    suggestions.forEach(cityData => {
      const cityCoord = {
        latitude: cityData.latitude,
        longitude: cityData.longitude
      };
      cityData.distanceInKM = geolib.getDistance(userCoord, cityCoord) / 1000; // distance in kilometers
    });
  }
};

const scoreSuggestions = suggestions => {
  suggestions.forEach(cityData => { cityData.score = score(cityData.distanceInKM, cityData.population); });
  sortByScore(suggestions);
  normaliseSuggestionScores(suggestions);
};

const score = (distance, population) => {
  let distanceScore = distance ? Math.pow(distance, -10 / 3) : 1;
  let populationScore = Math.pow(population, 6);
  return distanceScore * populationScore;
};

const sortByScore = suggestions => suggestions.sort((cityDataA, cityDataB) => cityDataB.score - cityDataA.score);

const normaliseSuggestionScores = suggestions => {
  const maxScore = suggestions.length >= 1 ? Math.log(suggestions[0].score) : null;
  const minScore = suggestions.length >= 1 ? Math.log(suggestions.slice(-1)[0].score) : null;

  // if min and max are even (happens when only 1 result, or possibly edge cases),
  // use max to get a score of 1 instead of dividing by 0
  const scoreDiff = (maxScore - minScore) || maxScore;

  suggestions.forEach(cityData => {
    cityData.score = Math.log(cityData.score);
    cityData.score -= minScore;
    cityData.score /= scoreDiff;
    cityData.score = Math.round(cityData.score * 100) / 100;
  });
};

module.exports = {};
module.exports.addDistanceToSuggestions = addDistanceToSuggestions;
module.exports.scoreSuggestions = scoreSuggestions;
module.exports.score = score;
module.exports.sortByScore = sortByScore;
module.exports.normaliseSuggestionScores = normaliseSuggestionScores;
