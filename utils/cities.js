const lineToMap = require("./tsv").lineToMap;
const lineReader = require("line-reader");
const { set, get, has } = require("lodash");

/** Business-aware utilities to construct the town index */
/** create object with these keys from a line */
const lineMapper = lineToMap([
  "id",
  "name",
  "ascii",
  "alt_name",
  "lat",
  "long",
  "feat_class",
  "feat_code",
  "country",
  "cc2",
  "admin1",
  "admin2",
  "admin3",
  "admin4",
  "population",
  "elevation",
  "dem",
  "tz",
  "modified_at"
]);

/** predictable and testable sanitized version of a given string */
function sanitizeString(str) {
  return str
    .toLowerCase()
    .replace(/[\s-_]+/g, "")
    .replace(/[éèêë]/g, "e")
    .replace(/[àâä]/g, "a")
    .replace(/[ïìî]/g, "i")
    .replace(/[üûù]/g, "u")
    .replace(/[ôòö]/g, "u");
}

function enrichIndexTree(tree, word, id) {
  let path = "";
  word.split().forEach((char, index) => {
    path += "." + char;
    if (index === word.length - 1) {
      set(tree, path, id);
    } else {
      if (!has(tree, path)) {
        set(tree, path, {});
      }
    }
  });
  return tree;
}

/** main function to index all the cities */
function indexCities(filePath) {
  return new Promise((resolve, reject) => {
    const db = {
      objects: {},
      cities: [],
      index: {}
    };
    let first = true;
    lineReader.eachLine(filePath, (line, last) => {
      // skip first line
      if (first) {
        first = false;
        return true;
      }
      const lineObject = lineMapper(line);

      const object = {
        lat: lineObject.lat,
        long: lineObject.long,
        name: lineObject.name
      };
      db.objects[lineObject.id] = object;
      db.cities.push(object);
      if (last) {
        resolve(db);
        return false;
      }
    });
  });
}

module.exports = {
  indexCities,
  sanitizeString
};
