const fs = require("fs");
let index = null;

/** load the index in memory */
function loadIndex(file) {
  index = JSON.parse(fs.readFileSync(file));
}

function suggest(query) {
  if (query !== "") {
  } else {
    return [];
  }
}

module.exports = {
  loadIndex
};
