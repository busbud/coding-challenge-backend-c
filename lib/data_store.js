
/**
 * Module dependencies.
 */

var _ = require('underscore');
var csv = require('fast-csv');
var diacritics = require('diacritics');
var geolib = require('geolib');


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
  var newDataStore = [];

  // Utility function to parse and store the info in a consistent fashion.
  var storeLine = function (dataLine) {
    // Ensure to set the ascii name to lower-case.
    newDataStore.push({
      id: dataLine.id,
      name: dataLine.name,
      ascii: dataLine.ascii.toLowerCase(),
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
 * Takes the list of grouped results and returns a single list of weighted and
 * properly formated results.
 *
 * @TODO: The scoring algorithm should be given as parameter by the caller.
 *
 * @param {Object} resultsIn List of grouped results.
 * @param {numer} maxResults The maximum number of resulst. Defaults to 10.
 * @return {Array} The list of results, sorted according to score. 
 * @api public
 */

function scoreResult(resultsIn, maxResults) {
  maxResults = maxResults || 10;
  var results = [];

  var formatData = function (cityData, score) {
    return {
      name: diacritics.remove(cityData.name + ', ' + cityData.admin1 + ', ' + cityData.country),
      latitude: cityData.latitude,
      longitude: cityData.longitude,
      score: score,
    };
  };

  _.each(resultsIn, function (cityDataList, delta) {
    // @FIXME: There are no way to break out of a _.each loop. Should look
    //         at alternate algorithm.
    if (results.length <= maxResults) {
      if (delta === 0) {
        results.push(formatData(cityDataList.shift(), 1.0));
        _.each(cityDataList, function (cityData) {
          results.push(formatData(cityData, 0.9));
        });
      } else {
        _.each(cityDataList, function (cityData) {
          results.push(formatData(cityData, 0.8));
        });
      }
    }
  });

  return results.slice(0, maxResults);
}


/**
 * Sorts a group of cities according the increasing distance between them
 * and a given geographical coordinate.
 *
 * @param {Array} cityDataList List of city data.
 * @param {Object} origin Point used to compute the distance. The object
 *                        needs a 'latitude' and 'longitude' data members.
 * @return {Array} A sorted list of the cities, containing an additional data
 *                 member 'distance'.
 * @api public
 */

function sortResultByDistance(cityDataList, origin) {
  return _.sortBy(cityDataList, function (cityData) {
    cityData.distance = geolib.getDistance(origin, cityData);
    return cityData.distance;
  });
}


/**
 * Sorts a group of cities according the decreasing population.
 *
 * @param {Array} cityDataList List of city data.
 * @return {Array} A sorted list of the cities.
 * @api public
 */

function sortResultByPopulation(cityDataList) {
  return _.sortBy(cityDataList, 'population').reverse();
}


/**
 * Sort each the results groups according to a give sorting function.
 * This function must take as input a city list and an optional argument.
 *
 * The given results list will be modified.
 *
 * @param {Object} results Dictionary of grouped results, indexed by the
 *                         'distance' between the city name used for the query
 *                         and the diffent results. The distance is the number
 *                         of letters that differs.
 * @param {Function} sortFunc Function to be used for sorting the sub-groups.
 * @param {Object} optParam Optional parameter for the sorting function.
 * @return {Object} The provided resuls object, with sub-group sorted.
 * @api public
 */

function sortResults(results, sortFunc, optParam) {
  _.each(results, function (cityDataList, delta) {
    results[delta] = sortFunc(cityDataList, optParam);
  });
  return results;
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
  var result = {};
  var itemLen = cityName.length;

  // Convert to lower-case for all string comparisons.
  cityName = diacritics.remove(cityName).toLowerCase();

  // Not a very efficient mean to lookup data.
  // @TODO: implement caching and use a DB for storage.
  _.each(dataStore, function (dataLine) {
    if (dataLine.ascii.slice(0, itemLen) === cityName) {
      var idx = dataLine.ascii.length - itemLen;
      if (result[idx]) {
        result[idx].push(dataLine);
      } else {
        result[idx] = [dataLine];
      }
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
  fipsCodeToProvinceCode: fipsCodeToProvinceCode,
  query: query,
  scoreResult: scoreResult,
  sortResults: sortResults,
  sortResultByPopulation: sortResultByPopulation,
  sortResultByDistance: sortResultByDistance,
};
