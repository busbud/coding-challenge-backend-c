const levenshtein = require("fast-levenshtein");

/** return the distance between two strings, useful for fuzzy matching */
function levenshteinDistance(text1, text2) {
  console.log("text1", text1, "text2", text2);
  return levenshtein.get(text1, text2);
}

/**
 * sort a bunch of values based on their (ascending) levenstein distance
 * extractor is function that can be used to get the actual value, if objects are used
 */
function sortByLevensteinDistance(pivot, predicates, extractor = id => id) {
  return predicates.sort((predicateA, predicateB) => {
    const valueA = extractor(predicateA);
    const valueB = extractor(predicateB);
    return levenshteinDistance(valueA, valueB);
  });
}

module.exports = {
  levenshteinDistance,
  sortByLevensteinDistance
};
