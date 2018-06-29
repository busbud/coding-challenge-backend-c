const score = require("string-score");

const METERS_IN_KM = 1000;
const WEIGHT_SCORING_DISTANCE = 0.7;
const WEIGHT_SCORING_NAME = 0.3;

module.exports = {
  scoreName: (text, pattern) => score(text, pattern, 0.8),
  scoreDistance: (distanceInMeters, radiusInKm) => {
    if (distanceInMeters === 0) {
      return 1;
    }

    let distanceInKm = distanceInMeters / METERS_IN_KM;
    return 1 - distanceInKm / radiusInKm;
  },
  scoring: ({ scoringName = 0, scoringDistance = null } = {}) => {
    return scoringDistance
      ? WEIGHT_SCORING_DISTANCE * scoringDistance + WEIGHT_SCORING_NAME * scoringName
      : scoringName;
  },
  sort: (a, b) => (a.score > b.score ? -1 : a.score === b.score ? 0 : 1)
};
