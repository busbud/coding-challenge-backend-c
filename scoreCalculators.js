const { getDistance } = require('./coordinatesUtils');
const { normalizeValues } = require('./normalizeValues');

const CIRCUMFERENCE_OF_EARTH = 40075;
const NAME_WEIGHT = 0.4;
const DISTANCE_WEIGHT = 0.6;

module.exports = { calculateSimpleScore, getDistanceBasedScoreCalculator };

// Originally the score field is the other way. 0 means confident, 1 is non confident, 
// thus we invert it for easier handling.
function calculateSimpleScore(city) {
  return 1 - city.score;
}

// Creates a calculator that calculates a score based on the name and the distance to a point. 
// The values are combined using a weighted score addition.
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
  // Distance score is just the distance from point A to point B in kilometers, normalized
  // to a value between 0 and 1.
  const distance = getDistance(userLatitude, userLongitude, cityLatitude, cityLongitude);
  // Considering no point can be further away than the circumference of the earth/2 (that means, the points are
  // exactly across the world), we can use CIRCUMFERENCE_OF_EARTH / 2 as the max possible value in the scale of distances,
  // making it possible to easily normalize distances without having to find out the min/max distances 
  const distanceScore = 1 - normalizeValues(distance, 0, CIRCUMFERENCE_OF_EARTH / 2);
  return distanceScore;
}