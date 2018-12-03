const { suggest } = require(".");
const { sanitizeString } = require("../text");

/**
 * search all list item without index
 */
function findMatchesInList(db, query) {
  const canonicalQuery = sanitizeString(query);
  return db.cities.filter(city => {
    return new RegExp(canonicalQuery).test(city.canonicalName);
  });
  return matches;
}

/**
 *
 */
const suggestFromList = suggest(findMatchesInList);

module.exports = {
  findMatchesInList,
  suggestFromList
};
