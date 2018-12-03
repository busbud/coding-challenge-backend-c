const lineReader = require("line-reader");
const { set, get, has } = require("lodash");
const { tsvLineToMap } = require("./tsv");
const provinceFips = require("../data/provinceFips.json");

// globals vars
const POPULATION_THRESHOLD = 5000;

/** Business-aware utilities to construct the town index */
/** create object with these keys from a line */
const lineMapper = tsvLineToMap(id => id, [
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
    .replace(/[ôòö]/g, "o");
}

/** is a given city valid in our search */
function isCityValid(city, threshold) {
  const population = parseInt(city.population);
  if (isNaN(population)) return false;
  return population > threshold;
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
      // the city is big enough
      if (isCityValid(lineObject, POPULATION_THRESHOLD)) {
        // get the province or state code
        const stateCode = provinceFips[lineObject.admin1] || lineObject.admin1;
        const object = {
          latitude: lineObject.lat,
          longitude: lineObject.long,
          name: `${lineObject.name}, ${stateCode}, ${lineObject.country}`,
          onlyName: lineObject.name
        };
        db.objects[lineObject.id] = object;
        db.cities.push(object);
      }
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
