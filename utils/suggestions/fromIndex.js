const { suggest } = require(".");
const { sanitizeString } = require("../text");

/** main search function in index */
function findMatchesInIndex(db, query) {
  if (query === "") return [];

  const sanitizedQuery = sanitizeString(query);
  // try to find
  const hasDirectMatchId = db.index.matches[sanitizedQuery];
  if (hasDirectMatchId) return db.objects[hasDirectMatchId];
  else {
    const partialMatches = db.index.partials[sanitizedQuery];

    return partialMatches.map(id => db.objects[id]);
  }
}

/**
 *
 */
const suggestFromIndex = suggest(findMatchesInIndex);

module.exports = {
  findMatchesInIndex,
  suggestFromIndex
};
