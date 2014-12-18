
var csv = require('fast-csv');
var _ = require('underscore');


var dataStore = [];


function setDataSource(source, callback) {
  var newDataStore = [];
  var storeLine = function (dataLine) {
    // Ensure to set the ascii name to lower-case.
    newDataStore.push({
      id: dataLine.id,
      ascii: dataLine.ascii.toLowerCase(),
      lat: dataLine.lat,
      long: dataLine.long,
      country: dataLine.country,
      admin1: dataLine.admin1,
      population: dataLine.population
    });
  };

  if (source.file) {
    csv
      .fromPath(source.file,  {headers: true, delimiter: '\t', quote: null})
      .on("data", storeLine)
      .on("end", function () {
        dataStore = newDataStore;
        callback(null);
      });
  } else if (source.string) {
    csv
      .fromString(source.string,  {headers: true})
      .on("data", storeLine)
      .on("end", function () {
        dataStore = newDataStore;
        callback(null);
      });
  } else {
    callback('Unknown or undefined data source');
  }
}


function query(item, callback) {
  var result = [];
  var itemLen = item.length;

  // Convert to lower-case for all string comparisons.
  item = item.toLowerCase();

  // Not a very efficient mean to lookup data.
  // @TODO: implement caching and use a DB for storage.
  _.each(dataStore, function (dataLine) {
    if (dataLine.ascii.slice(0, itemLen) === item) {
      result.push(dataLine);
    }
  });
  callback(null, result);
}


module.exports = {
  setDataSource: setDataSource,
  query: query,
};
