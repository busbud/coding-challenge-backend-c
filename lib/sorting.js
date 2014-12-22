
/**
 * Module dependencies.
 */

var _ = require('underscore');
var geolib = require('geolib');


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

function sortResultsByDistance(cityDataList, origin) {
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

function sortResultsByPopulation(cityDataList) {
  return _.sortBy(cityDataList, 'population').reverse();
}


/**
 * Sort each the results groups according to a give sorting function.
 * This function must take as input a city list and an optional argument.
 *
 * @param {Object} results Dictionary of grouped results, indexed by the
 *                         'distance' between the city name used for the query
 *                         and the diffent results. The distance is the number
 *                         of letters that differs.
 * @param {Function} sortFunc Function to be used for sorting the sub-groups.
 * @param {Object} optParam Optional parameter for the sorting function.
 * @return {Object} The results object, with sub-group sorted.
 * @api public
 */

function sortResults(resultsIn, sortFunc, optParam) {
  var results = {};
  _.each(resultsIn, function (cityDataList, delta) {
    results[delta] = sortFunc(cityDataList, optParam);
  });
  return results;
}


/**
 * Exports.
 */

module.exports = {
  sortResults: sortResults,
  sortResultsByPopulation: sortResultsByPopulation,
  sortResultsByDistance: sortResultsByDistance,
};
