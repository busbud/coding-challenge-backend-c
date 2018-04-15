/**
 * Load, parse, cache, and provide utility methods to deal with cities.
 * The list of cities are taken from the `cities.tsv` resource.
 * The list of admin codes ASCII representation are taken from `admincodes.json` resource.
 * 
 * @author Mohammad Fares <faressoft.com@gmail.com>
 */

/**
 * The path of the texts file
 * 
 * @type {String}
 */
var CITIES_FILE_NAME = __dirname + '/../resources/cities.tsv';

/**
 * The path of the texts file
 * 
 * @type {String}
 */
var ASCII_ADMIN_CODES_FILE_NAME = __dirname + '/../resources/admincodes.json';

/**
 * To store the loaded cities
 * @type {Array} [{name, title, latitude, longitude, population, nameFreq}]
 */
var cities = [];

/**
 * To store the loaded ASCII admin codes
 * @type {Object} {countryCode: {id: code, ...}}
 */
var adminCodes = {};

/**
 * The cities are loaded
 * @type {Boolean}
 */
var loaded = false;

/**
 * Get the ASCII representation of the admin code
 * 
 * @param  {String} countryCode
 * @param  {String} adminId
 * @return {String} return the same passed id if not found
 */
function getAdminCode(countryCode, adminId) {

  // The country code is not exist
  if (di.is.not.propertyDefined(adminCodes, countryCode)) {
    return adminId;
  }

  // The admin code is not exist
  if (di.is.not.propertyDefined(adminCodes[countryCode], adminId)) {
    return adminId;
  }

  return adminCodes[countryCode][adminId];

}

/**
 * Prase a list of raw city records. Each city record's is a tab seperated values record that
 * has the following attrbiutes:
 * 
 * - geonameid
 * - name
 * - asciiname
 * - alternatenames
 * - latitude
 * - longitude
 * - feature_class
 * - feature_code
 * - country_code
 * - cc2
 * - admin1_code
 * - admin2_code
 * - admin3_code
 * - admin4_code
 * - population
 * - elevation
 * - dem
 * - timezone
 * - modification_date
 * 
 * @param  {Array} record
 * @return {Array} [{name, title, latitude, longitude, population}, ...]
 */
function parseCityRecord(record) {

  var city = {};

  var ASCIINAME = 2;
  var LATITUDE = 4;
  var LONGITUDE = 5;
  var COUNTRY_CODE = 8;
  var ADMIN1_CODE = 10;
  var POPULATION = 14;

  // Prase into records
  record = record.split('\t');

  // ASCII representation of the admin code
  var adminCode = getAdminCode(record[COUNTRY_CODE], record[ADMIN1_CODE]);

  // Pick the needed attributes
  city = {
    name: record[ASCIINAME],
    title: record[ASCIINAME] + ', ' + adminCode + ', ' + record[COUNTRY_CODE],
    latitude: parseFloat(record[LATITUDE]),
    longitude: parseFloat(record[LONGITUDE]),
    population: parseInt(record[POPULATION])
  };

  return city;

}

/**
 * Check if a passed city has a population above a passed n
 * 
 * @param  {Number}  n
 * @param  {Object}  city {population}
 * @return {Boolean}
 */
function hasPopulationAbove(n, city) {

  return city.population > n;

}

/**
 * Get a list of cities
 *
 * @param  {Number} populationLowerBoundFilter filter the cities
 *                                             that have gte population (Optional)
 * @return {Array}  [{name, title, latitude, longitude, population, nameFreq}]
 */
function getCities(populationLowerBoundFilter) {

  // Cities are not loaded first
  if (!loaded) {
    throw new Error('Citites are not loaded yet');
  }

  // No filter for population
  if (typeof populationLowerBoundFilter == 'undefined') {
    return cities;
  }

  return di.lodash.filter(cities, function(city) {
    return city.population >= populationLowerBoundFilter;
  });

}

/**
 * Load, parse, and cache the cities names
 * 
 * @return {Promise}
 */
function load() {

  return new Promise(function(resolve, reject) {

    // Load the citiies file (tab seperated file)
    di.fs.readFile(CITIES_FILE_NAME, {encoding: 'utf8'}, function(error, citiesContent) {

      if (error) {
        return reject(error);
      }

      // Load the admin codes file
      di.fs.readFile(ASCII_ADMIN_CODES_FILE_NAME, {encoding: 'utf8'}, function(error, adminCodeContent) {

        if (error) {
          return reject(error);
        }

        // Parse the adminCodeContent
        adminCodes = JSON.parse(adminCodeContent);

        // Remove trailing whitespaces
        cities = di.lodash.trim(citiesContent);

        // Parse into raw records
        cities = cities.split('\n');

        // Remove the first line (attribute names)
        cities.splice(0, 1);

        // Prase the records into fields
        cities = di.lodash.map(cities, parseCityRecord);

        // The cities are loaded
        loaded = true;

        resolve();
        
      });

    });

  });
  
}

////////////////////////////////////////////////////
// Module //////////////////////////////////////////
////////////////////////////////////////////////////

module.exports = {
  load: load,
  getCities: getCities
};
