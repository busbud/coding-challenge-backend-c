var _ = require('lodash');
var LatLon = require('../vendor/lat_lon');

var SCORE_GEO_WEIGHT = 5;
var SCORE_POP_WEIGHT = 1;
var SCORE_TOTAL_WEIGHT = SCORE_POP_WEIGHT + SCORE_GEO_WEIGHT;
var FURTHEST_DISTANCE = 19200;
var POP_CAP = 3000000;

function Scorer() {
}

Scorer.prototype._attributeScore = function(result, params) {
  // Let's start the score at 0 and ad weithed scores to it.
  result.score = 0;

  if (params.latitude && params.longitude) {

    // Distance score
    var origin = new LatLon(params.latitude, params.longitude);
    var result_location = new LatLon(result.lat, result.long);
    var distance = origin.distanceTo(result_location);
    var distance_score = (1-(distance / FURTHEST_DISTANCE));

    result.score += distance_score * (SCORE_GEO_WEIGHT / SCORE_TOTAL_WEIGHT);

  } else {

    // Hey we can calculate this geo score without origin lat/lng
    // Everybody has 100% on this test
    result.score += 1 * (SCORE_GEO_WEIGHT / SCORE_TOTAL_WEIGHT);

  }

  // Population score
  var population_score = (Math.min(result.population, POP_CAP) / POP_CAP);
  result.score += population_score * (SCORE_POP_WEIGHT / SCORE_TOTAL_WEIGHT);

  // Cap the amount of numbers after the comma to 6
  result.score = (Math.round(result.score.toFixed(6) * 1000000) / 1000000);

  return result;
};

Scorer.prototype.score = function(results, params) {
  var self = this;

  // Go and calculate individual scores
  var scored_results = _.map(results, function(result) {
    return self._attributeScore(result, params);
  });

  // Sort results by score
  var sorted_scored_results = Array.prototype.sort.call(scored_results, function(r1, r2) {
    return r2.score - r1.score;
  });

  return sorted_scored_results;
};

module.exports = Scorer;
