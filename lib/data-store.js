
/**
 * Module dependencies.
 */

var _ = require('underscore');
var cacheManager = require('cache-manager');
var csv = require('fast-csv');
var diacritics = require('diacritics');
var Triejs = require('triejs');


/**
 * Module variables.
 */

var cacheSize = process.env.CACHE_SIZE || 1000; /* items, LRU */
var cacheTtl = process.env.CACHE_TTL || 3600; /* seconds */
var memoryCache = cacheManager.caching({
  store: 'memory',
  max: cacheSize,
  ttl: cacheTtl
});
var dataStore = new Triejs();


/**
 * Get the English name for the given country ISO 2 code string.
 * http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
 *
 * Given an unknown code string, the same string is returned.
 *
 * @param {String} coutnryCode
 * @return {String} the country name
 * @api public
 */

function countryCodeToName(countryCode) {
  // @TODO: read the data from an external source / DB
  var countryMap = {
    'CA': 'Canada',
    'US': 'USA',
  };
  return countryMap[countryCode] || countryCode;
}


/**
 * Get the 2 letter code for a Canadian province.
 * The fips code is expected to be the two numbers of the region code.
 * http://en.wikipedia.org/wiki/List_of_FIPS_region_codes
 * http://en.wikipedia.org/wiki/Canadian_postal_abbreviations_for_provinces_and_territories
 *
 * An unkown fips code will cause that code to be returned.
 *
 * @param {String} fipsCode
 * @return {String} the province two-letter code if found, code given otherwise.
 * @api public
 */

function fipsCodeToProvinceCode(fipsCode) {
  // @TODO: read the data from an external source / DB
  var regionMap = {
    '01': 'AB',
    '02': 'BC',
    '03': 'MB',
    '04': 'NB',
    '05': 'NL',
    '07': 'NS',
    '08': 'ON',
    '09': 'PE',
    '10': 'QC',
    '11': 'SK',
    '12': 'YT',
    '13': 'NT',
    '14': 'NU',
  };
  return regionMap[fipsCode] || fipsCode;
}


/**
 * Set the data source with all of the cities information.
 * The source can either be a file or a string 
 *
 * @param {Object} source source.file with filename, or source.string
 * @param {Function} callback
 * @api public
 */

function setDataSource(source, callback) {

  // Temporary holder for the new data source.
  var newDataStore = new Triejs({maxCache: 30});

  // Utility function to parse and store the info in a consistent fashion.
  var storeLine = function (dataLine) {

    var asciiName = dataLine.ascii.toLowerCase();

    newDataStore.add(asciiName, {
      id: dataLine.id,
      name: dataLine.name,
      ascii: asciiName,
      latitude: dataLine.lat,
      longitude: dataLine.long,
      country: countryCodeToName(dataLine.country),
      admin1: fipsCodeToProvinceCode(dataLine.admin1),
      population: parseInt(dataLine.population, 10),
    });
  };

  if (source.file) {
    csv
      .fromPath(source.file,  {headers: true, delimiter: '\t', quote: null})
      .on("data", storeLine)
      .on("end", function () {
        // Swap the old data store for the new one.
        dataStore = newDataStore;
        callback(null);
      });
  } else if (source.string) {
    csv
      .fromString(source.string,  {headers: true})
      .on("data", storeLine)
      .on("end", function () {
        // Swap the old data store for the new one.
        dataStore = newDataStore;
        callback(null);
      });
  } else {
    callback('Unknown or undefined data source');
  }
}


/**
 * Query our database for a city.
 *
 * The results returned are grouped according to the number of letters that
 * are different between the searched name and the results.
 *
 * @TODO: The grouping should be done according to a function given by
 *        the caller.
 *
 * @param {String} cityName
 * @param {Function} callback
 * @api public
 */

function query(cityName, callback) {
  var results = {};
  var itemLen = cityName.length;

  // Convert to lower-case for all string comparisons.
  cityName = diacritics.remove(cityName).toLowerCase();

  memoryCache.wrap(cityName, function (cacheCb) {
    _.each(dataStore.find(cityName), function (cityData) {
      var idx = cityData.ascii.length - itemLen;
      if (results[idx]) {
        results[idx].push(cityData);
      } else {
        results[idx] = [cityData];
      }
    });
    cacheCb(null, results);
  }, callback);

}


/**
 * Exports.
 */

module.exports = {
  setDataSource: setDataSource,
  countryCodeToName: countryCodeToName,
  fipsCodeToProvinceCode: fipsCodeToProvinceCode,
  query: query,
};
