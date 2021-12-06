
var distanceScore = require('./scores/distance.js');
var exactMatchScore = require('./scores/exact-match.js');
var popularityScore = require('./scores/popularity.js');
var stateScore = require('./scores/state.js');
var nearMatch = require('./scores/near-match.js');
var config = require('./config.json');

module.exports = [
  {f:distanceScore, weight: config.score.distance},
  {f:exactMatchScore, weight: config.score['exact-match']},
  {f:popularityScore, weight: config.score.popularity},
  {f:stateScore, weight: config.score.state},
  {f:nearMatch, weight: config.score['near-match']}
]