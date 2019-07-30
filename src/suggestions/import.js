/**
 * !! IMORTANT !!
 *
 * If you need to reuse this script to update the current list of cities imported
 * during the first migration, please implement some logic in streamToMongoBulkWrite in order
 * for it to accept an (upsert = upsert || false) argument so it can build the write queries
 * accordingly.
 *
 */
const {
  streamMap,
  streamFilter,
  streamToMongoBulkWrite
} = require("./../transforms");
const { composeStream } = require("./../fp");
const { stateCodeAbbrv } = require("./../suggestions/geo");
const csv = require("csv-parser");
const fs = require("fs");
const csvStream = fs.createReadStream("./data/cities_canada-usa.tsv");
const COLLECTION = "locations";

/**
 * Only keep cities with a population above 5000. Impactless for
 * now but it would allow to reuse the same script when the list of
 * locations get updated.
 *
 * @param {object} row
 * @return {boolean}
 */
const discardSmallCity = row => row.population > 5000;

/**
 * Set a user friendly name for each of the locations during the import
 * to avoid processing the data on every api request.
 * 
 * @param {object}
 * @returns {object}
 */
const setDisambiguateName = row => {
  row.disambiguateName = `${row.ascii}, ${stateCodeAbbrv(
    row.country,
    row.admin1
  )}, ${row.country}`;
  return row;
};

/**
 * Add the geo field for the 2dsphere index.
 * 
 * @param {object} row 
 * @returns {object}
 */
const setGeoField = (row) => {
  row.testGeo = {
    type: "Point",
    coordinates: [Number(row.long), Number(row.lat)],
  }

  return row;
}

/**
 * Set a searchField which consists of the ascii field without spaces.
 * Allows to test against locations without taking accents and
 * spaces into account.
 * 
 * @param {object} row
 * @returns {object}
 */
const setSearchField = row => {
  row.searchField = row.ascii.replace(/\s/, "").toLowerCase();
  return row;
};


/**
 * @param {object} row 
 * @returns {object}
 */
const castPopulationToNumber = row => {
  row.population = Number(row.population)
  return row;
};

/**
 * Bulkwrite prepared documents to mongo.
 *
 * @param {Db} mongo
 * @param {string} collectionName
 * @returns {Promise}
 */
const write = (mongo, collectionName) => {
  return documents => {
    return mongo.collection(collectionName).bulkWrite(documents);
  };
};

/**
 * Stream data from the tsv to mongo using transform streams. The implementation
 * of 'streamToMongoBulkWrite' is made in such a way that it will emit the "end" event
 * only once all promises returned from it's callback are resolved. It make it easier
 * to return a promise to the migration runner.
 *
 * @param {Db} mongo
 * @returns {Promise}
 */
const runUp = mongo => {
  return new Promise((resolve, reject) => {
    composeStream(
      csv({ separator: "\t", quote: "", strict: true }),
      streamMap(setDisambiguateName),
      streamMap(setSearchField),
      streamMap(castPopulationToNumber),
      streamMap(setGeoField),
      streamFilter(discardSmallCity),
      streamToMongoBulkWrite(write(mongo, COLLECTION))
    )(csvStream)
      .on("data", () => {})
      .on("error", err => reject(err))
      .on("end", () => {        
        resolve();
      });
  });
};

exports.runUp = runUp;
exports.COLLECTION = COLLECTION;
