const fs = require("fs");
let index = null;

/** load the index in memory for the sake of example */
function loadIndex(file) {
  const content = fs.readFileSync(file);
  index = JSON.parse(fs.readFileSync(file, "utf-8"));
}

/** main search function in index */
function matchesFor(query) {
  if (query === "") return [];
  const directResult = index.index[query];

  return directResult ? index.objects[directResult.id] : [];
}

/** the main suggest function */
function suggest(query) {
  console.log("about to suggest for this function", query);
  if (query !== "") {
    const matches = matchesFor(query);
    return {
      suggestions: matches
    };
  } else {
    return { suggestions: [] };
  }
}

module.exports = {
  loadIndex,
  suggest
};
