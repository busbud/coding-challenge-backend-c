/**
 * Module dependencies.
 */

var _ = require('underscore');
var diacritics = require('diacritics');


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

function scoreResults(resultsIn, maxResults) {
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
 * Exports.
 */

module.exports = {
  scoreResults: scoreResults,
};
