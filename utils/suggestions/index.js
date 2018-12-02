const fs = require("fs");

/** load the index in memory for the sake of example */
function loadIndex(file) {
  const content = fs.readFileSync(file);
  return JSON.parse(fs.readFileSync(file, "utf-8"));
}

module.exports = {
  loadIndex
};
