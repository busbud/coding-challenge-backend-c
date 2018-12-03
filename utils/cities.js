const lineReader = require("line-reader");
const { set, get } = require("lodash");
const { tsvLineToMap } = require("./tsv");
const { sanitizeString } = require("./text");
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
      index: {
        matches: {},
        partials: {}
      }
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
          id: lineObject.id,
          latitude: lineObject.lat,
          longitude: lineObject.long,
          name: `${lineObject.name}, ${stateCode}, ${lineObject.country}`,
          canonicalName: sanitizeString(lineObject.name),
          onlyName: lineObject.name
        };
        // buid a text index
        buildTextIndex(db, object);
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

/**
 * build a text index with direct matches and partial matches with ids list
 */
function buildTextIndex(
  db = { index: { matches: {}, partials: {} } },
  predicate
) {
  const id = predicate.id;
  const chars = predicate.canonicalName.split("");
  let path = "";

  // register the direct match
  db.index.matches[predicate.canonicalName] = id;
  // redirect partial matches
  for (let char of chars) {
    path += char;
    const ids = get(db.index.partials, path) || [];
    ids.push(id);
    set(db.index.partials, path, ids);
  }
  return db;
}

module.exports = {
  indexCities,
  sanitizeString,
  isCityValid,
  buildTextIndex
};
