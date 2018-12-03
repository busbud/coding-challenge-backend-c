const { suggest } = require(".");

/** main search function in index */
function findMatchesInIndex(db, query) {
  if (query === "") return [];

  // try to find
  const hasDirectMatchId = db.index.matches[query];
  if (hasDirectMatchId) return db.objects[hasDirectMatchId];
  else {
    const partialMatches = db.index.partials[query];
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
