
/**
 * Module dependencies.
 */

var csv = require('fast-csv');
var diacritics = require('diacritics');
var _ = require('underscore');


/**
 * Module variables.
 */

var dataStore = [];


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
 * Get the English name for state/province.
 * The fips code is expected to be the two letter code concanated with the
 * two numbers of the region code.
 * http://en.wikipedia.org/wiki/List_of_FIPS_region_codes
 *
 * For the US, the two letter state code is used instead of the number.
 *
 * An unkown fips code will cause that code to be returned.
 *
 * @param {String} fipsCode
 * @return {String} the state/province name, or the fips code given if error
 * @api public
 */

function fipsCodeToName(fipsCode) {
  // @TODO: read the data from an external source / DB
  var regionMap = {
    'CA01': 'Alberta',
    'CA02': 'British Columbia',
    'CA03': 'Manitoba',
    'CA04': 'New Brunswick',
    'CA05': 'Newfoundland and Labrador',
    'CA07': 'Nova Scotia',
    'CA08': 'Ontario',
    'CA09': 'Prince Edward Island',
    'CA10': 'Quebec',
    'CA11': 'Saskatchewan',
    'CA12': 'Yukon',
    'CA13': 'Northwest Territories',
    'CA14': 'Nunavut',
    'USAK': 'Alaska',
    'USAL': 'Alabama',
    'USAR': 'Arkansas',
    'USAZ': 'Arizona',
    'USCA': 'California',
    'USCO': 'Colorado',
    'USCT': 'Connecticut',
    'USDC': 'Washington, D.C.',
    'USDE': 'Delaware',
    'USFL': 'Florida',
    'USGA': 'Georgia',
    'USHI': 'Hawaii',
    'USIA': 'Iowa',
    'USID': 'Idaho',
    'USIL': 'Illinois',
    'USIN': 'Indiana',
    'USKS': 'Kansas',
    'USKY': 'Kentucky',
    'USLA': 'Louisiana',
    'USMA': 'Massachusetts',
    'USMD': 'Maryland',
    'USME': 'Maine',
    'USMI': 'Michigan',
    'USMN': 'Minnesota',
    'USMO': 'Missouri',
    'USMS': 'Mississippi',
    'USMT': 'Montana',
    'USNC': 'North',
    'USND': 'North',
    'USNE': 'Nebraska',
    'USNH': 'New',
    'USNJ': 'New',
    'USNM': 'New',
    'USNV': 'Nevada',
    'USNY': 'New',
    'USOH': 'Ohio',
    'USOK': 'Oklahoma',
    'USOR': 'Oregon',
    'USPA': 'Pennsylvania',
    'USRI': 'Rhode',
    'USSC': 'South',
    'USSD': 'South',
    'USTN': 'Tennessee',
    'USTX': 'Texas',
    'USUT': 'Utah',
    'USVA': 'Virginia',
    'USVT': 'Vermont',
    'USWA': 'Washington',
    'USWI': 'Wisconsin',
    'USWV': 'West',
    'USWY': 'Wyoming',
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
  var newDataStore = [];

  // Utility function to parse and store the info in a consistent fashion.
  var storeLine = function (dataLine) {
    // Ensure to set the ascii name to lower-case.
    newDataStore.push({
      id: dataLine.id,
      name: dataLine.name,
      ascii: dataLine.ascii.toLowerCase(),
      lat: dataLine.lat,
      long: dataLine.long,
      country: countryCodeToName(dataLine.country),
      admin1: fipsCodeToName(dataLine.country + dataLine.admin1),
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


/**
 * Query our database for a city.
 *
 * @param {String} cityName
 * @param {Function} callback
 * @api public
 */

function query(cityName, callback) {
  var result = [];
  var itemLen = cityName.length;

  // Convert to lower-case for all string comparisons.
  cityName = diacritics.remove(cityName).toLowerCase();

  // Not a very efficient mean to lookup data.
  // @TODO: implement caching and use a DB for storage.
  _.each(dataStore, function (dataLine) {
    if (dataLine.ascii.slice(0, itemLen) === cityName) {
      result.push(dataLine);
    }
  });
  callback(null, result);
}


/**
 * Exports.
 */

module.exports = {
  setDataSource: setDataSource,
  countryCodeToName: countryCodeToName,
  fipsCodeToName: fipsCodeToName,
  query: query,
};
