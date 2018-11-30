const fs = require("fs");
let index = null;

/** load the index in memory */
function loadIndex(file) {
  const content = fs.readFileSync(file);
  index = JSON.parse(fs.readFileSync(file, "utf-8"));
}

function suggest(query) {
  if (query !== "") {
    return {
      suggestions: [index.index[query]]
    };
  } else {
    return { suggestions: [] };
  }
}

module.exports = {
  loadIndex,
  suggest
};
