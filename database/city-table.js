// Internal dependencies
var _ = require('../utils/underscore_with_string');
var logger = require('../utils/logger');
var utilMethods = require('../utils/util_methods');


// External dependencies
var tsvParser = require("node-tsv-json");
var fs = require('fs');


var cityTable = exports;


// Our table with records
var cities = [];

// Possible state
var DB_STATE = {
  EMPTY: 0,
  BUSY: 1,
  UP: 2
};

// Db state is first empty
var CURRENT_DB_STATE = DB_STATE.EMPTY;


// Is database seeded and up ?
function isDatabaseUp() {
  // Be sure we return a bool value
  var isDbUp = (CURRENT_DB_STATE === DB_STATE.UP);
  return isDbUp;
}


// Seed database
function populateDatabase(source, destination) {

  // If data has already been parsing
  // or is being parsed just return
  var isDbUp = isDatabaseUp();
  if (isDbUp) {
    return;
  }

  if (_.str.isBlank(source) ||
    _.str.isBlank(destination)) {
    throw new Error('You have to set a source and destination db file');
  }

  var parserOpts = {
    input: source,
    output: null,
    parseRows: false
  };

  tsvParser(parserOpts,
    function(err, result) {

      if (err) {
        logger.error('CityTable#populateDatabase -- ' +
          'An error occured during the parsing of the .tsv');
        throw err;
      }

      // We got the array and we can sort it :)
      cities = _.sortBy(result, 'name');

      // We can now process request
      CURRENT_DB_STATE = DB_STATE.UP;

    });

}

function findByPrefix(prefix, callback) {

  var err = null;

  if (_.str.isBlank(prefix)) {
    err = new Error('CityTable#findByPrefix -- No prefix given');
    callback(err, null);
    return;
  }

  var is_callbac_a_func = (_.isFunction(callback) === true);

  if (!is_callbac_a_func) {
    err = new Error('CityTable#findByPrefix -- callback is not a function');
    callback(err, null);
    return;
  }

  // Don't let matches go away because of case
  var beginWith = utilMethods.getLowerCaseUnidecodedString(prefix);

  var matches = _.filter(cities, function(city) {

    var cityName = utilMethods.getLowerCaseUnidecodedString(city.name);
    var beginWithPrefix = _.str.startsWith(cityName, beginWith);

    return beginWithPrefix;

  });


  callback(err, matches);

}

// Init stuff
cityTable.populateDatabase = populateDatabase;
cityTable.isDatabaseUp = isDatabaseUp;


// Db stuff
cityTable.findByPrefix = findByPrefix;
