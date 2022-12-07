const stringsimilarity = require("string-similarity");

/**
 * Gets city name score
 * @param {*} string1 first string
 * @param {*} string2 second string
 * @returns {Number}
 */
module.exports = (string1, string2) => {
  return Number(
    Number(
      stringsimilarity.compareTwoStrings(string1, string2).toFixed(1)
    ).toFixed(1)
  );
};
