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

  var score = 1.0;
  _.each(resultsIn, function (cityDataList, delta) {
    // @TODO: There are no way to break out of a _.each loop. Should look
    //         at alternate algorithm.
    delta = parseInt(delta, 10);
    if (results.length <= maxResults) {
      if (delta === 0) {
        // For exact matches, the first one will be seen as a 'perfect' match.
        // While the others will be nearly so, but not quite.
        results.push(formatData(cityDataList.shift(), score));
        score = 0.9;
        _.each(cityDataList, function (cityData) {
          results.push(formatData(cityData, score));
        });
      } else if (delta > 0 && delta <= 3) {
        // When we have between 1 and 3 letters different, it is not a perfect
        // match. The first one is 'promoted', the others are lower
        score = Math.min(score, 0.9);
        results.push(formatData(cityDataList.shift(), score));
        score = Math.max(score - 0.4, 0.2);
        _.each(cityDataList, function (cityData) {
          results.push(formatData(cityData, score));
        });
      } else {
        // Otherwise, for every new categories, we lower the score even more.
        score = Math.max(score - 0.2, 0.1);
        _.each(cityDataList, function (cityData) {
          results.push(formatData(cityData, score));
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
