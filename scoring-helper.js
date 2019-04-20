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
      cityData.distanceInKM = geolib.getDistance(userCoord, cityCoord) / 1000; //distance in kilometers
    })
  }
};

const scoreSuggestions = suggestions => {
  suggestions.forEach(cityData => cityData.score = score(cityData.distanceInKM, cityData.population));
  suggestions.sort((cityDataA, cityDataB) => cityDataB.score - cityDataA.score);
  const maxScore = suggestions.length >= 1 ? Math.log(suggestions[0].score) : null;
  const minScore = suggestions.length >= 1 ? Math.log(suggestions.slice(-1)[0].score) : null;
  suggestions.forEach(cityData => {
    cityData.score = Math.log(cityData.score);
    cityData.score -= minScore;
    cityData.score /= (maxScore - minScore);
    cityData.score = Math.round(cityData.score * 100) / 100;
  });
};

const score = (distance, population) => Math.pow(distance, -10 / 3) * Math.pow(population, 6);

module.exports = {};
module.exports.addDistanceToSuggestions = addDistanceToSuggestions;
module.exports.scoreSuggestions = scoreSuggestions;

