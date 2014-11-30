'use strict';

var GEO_WEIGHT = 5,
    POP_WEIGHT = 3,
    NAME_WEIGHT = 2,
    NAME_MAX_PENALITY = 0.10,
    TOTAL_WEIGHT = POP_WEIGHT + GEO_WEIGHT + NAME_WEIGHT;

// @param {string} name
// @param {string} prefix
// @return {number}
var scoreName = function(name, prefix) {
  var score = 1;
  var diff = name.length - prefix.length;
  if(diff > 3) {
    var penalty = (diff * NAME_MAX_PENALITY) / 10;
    score = (Math.min(penalty, NAME_MAX_PENALITY) / NAME_MAX_PENALITY);
  }
  return score;
}

// @param {Object} city
// @param {number} maxPopulation
// @param {number} maxDistance
// @param {string} prefix
// @return {number}
module.exports = function calculateScore(city, maxPopulation, maxDistance, prefix){
    var score = 0;
    if(city.distance){
      var distance_score = (1-(city.distance / maxDistance));
      score += distance_score * (GEO_WEIGHT / TOTAL_WEIGHT);
    } else {
      // no geoloc
      score += 1 * (GEO_WEIGHT / TOTAL_WEIGHT);
    }

    var population_score = (Math.min(city.population, maxPopulation) / maxPopulation);
    score += population_score * (POP_WEIGHT / TOTAL_WEIGHT);

    var name_score = scoreName(city.rawName, prefix);
    score += name_score * (NAME_WEIGHT / TOTAL_WEIGHT);

    return Math.round(score * 10) / 10;
} 
