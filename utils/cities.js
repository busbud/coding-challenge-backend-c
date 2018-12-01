const lineToMap = require("./tsv").lineToMap;
const lineReader = require("line-reader");

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
  return str.replace(/\s+/g, "").toLowerCase();
}

/** main function to index all the cities */
function indexCities(filePath) {
  return new Promise((resolve, reject) => {
    const db = {
      objects: {},
      index: {}
    };
    let first = true;
    lineReader.eachLine(filePath, (line, last) => {
      // skip first line
      if (first) return true;
      const lineObject = lineMapper(line);
      db.objects[lineObject.id] = {
        lat: lineObject.lat,
        long: lineObject.long,
        name: lineObject.name
      };
      db.index[lineObject.name] = {
        id: lineObject.id
      };
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
