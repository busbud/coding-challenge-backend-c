const getDistanceScore = require("./getDistanceScore");
const getNameScore = require("./getNameScore");

/**
 * Gets Query score
 * @param {*} string1 first string
 * @param {*} string2 second string
 * @param {Object} from location from
 * @param {Object} to  location to
 * @returns {Number} the score
 */
module.exports = (string1, string2, from, to) =>
  !from?.latitude || !from?.longitude || !to.latitude || !to.longitude
    ? getNameScore(string1, string2)
    : Number(
        (
          (getNameScore(string1, string2).toFixed(1) +
            getDistanceScore(from, to)) /
          2
        ).toFixed(1)
      );
