const levenshtein = require("fast-levenshtein");

/** predictable and testable sanitized version of a given string */
function sanitizeString(str) {
  return str
    .toLowerCase()
    .replace(/[\s-_]+/g, "")
    .replace(/[éèêë]/g, "e")
    .replace(/[àâä]/g, "a")
    .replace(/[ïìî]/g, "i")
    .replace(/[üûù]/g, "u")
    .replace(/[ôòö]/g, "o");
}

/**
 * return the distance between two strings, useful for fuzzy matching
 * @param {string} text1
 * @param {string} text2
 */
function levenshteinDistance(text1, text2) {
  return levenshtein.get(text1, text2);
}

module.exports = {
  levenshteinDistance,
  sanitizeString
};
