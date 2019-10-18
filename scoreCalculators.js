const { getDistance } = require('./coordinatesUtils');
const { normalizeValues } = require('./normalizeValues');

const CIRCUMFERENCE_OF_EARTH = 40075;
const NAME_WEIGHT = 0.4;
const DISTANCE_WEIGHT = 0.6;

module.exports = { calculateSimpleScore, getDistanceBasedScoreCalculator };

function calculateSimpleScore(city) {
  return 1 - city.score;
}

function getDistanceBasedScoreCalculator(userLatitude, userLongitude) {
  return (city) => {
    const initialScore = calculateSimpleScore(city);
    const distanceScore = calculateDistancesScore(
      Number(city.item.lat), 
      Number(city.item.long), 
      Number(userLatitude), 
      Number(userLongitude)
    );

    return initialScore * NAME_WEIGHT + distanceScore * DISTANCE_WEIGHT;
  };
}

function calculateDistancesScore(cityLatitude, cityLongitude, userLatitude, userLongitude) {
  const distance = getDistance(userLatitude, userLongitude, cityLatitude, cityLongitude);
  // Distance score is just the distance from point A to point B in kilometers, normalized
  // to a value between 0 and 1.
  const distanceScore = 1 - normalizeValues(distance, 0, CIRCUMFERENCE_OF_EARTH / 2);
  return distanceScore;
}