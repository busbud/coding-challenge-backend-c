const getDistanceInKm = require("./getDistanceInKm");
/**
 * Gets proximity score between two locations
 * @param {Object} from location from
 * @param {Object} to  location to
 * @returns {Number} the score
 */
module.exports = (from, to) => {
  if (!from || !to) return 0;
  const distanceKm = getDistanceInKm(from, to);
  let score = 1000 - distanceKm;
  score = score > 0 ? Math.round(score / 10) / 100 : 0;
  return score;
};
