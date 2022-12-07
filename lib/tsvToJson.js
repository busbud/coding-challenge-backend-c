const { readFileSync } = require("fs");
const tsv = readFileSync("data/cities_canada-usa.tsv");
/**
 * Converts tsv file to JSON
 * @returns {Object}
 */
function tsvToJson() {
  const lines = tsv.toString().split("\n");
  const result = [];
  const headers = lines[0].split("\t");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split("\t");

    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }

    result.push(obj);
  }

  return result;
}

module.exports = tsvToJson;
