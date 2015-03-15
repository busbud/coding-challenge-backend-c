
var levenshtein = require('fast-levenshtein');
var rmDiacritics = require('diacritics').remove;

var textScoreWeight = 0.7;
var distanceScoreWeight = 0.3;

function getLevenhSteinBasedScore(str1, str2) {
  // levenshtein of 0 = equality
  // the max levenshtein score is the max length of the 2 compared strings
  var levenshteinDistance = levenshtein.get(str1, str2);
  var maxLength = Math.max(str1.length, str2.length);
  var minLength = Math.min(str1.length, str2.length);
  // not bad... but not perfect... because levenshtein score is low is str2.length << str1 ("Mtl" example)
  var textScore =  (str1.length / str2.length) * ((maxLength - levenshteinDistance ) * (maxLength - levenshteinDistance )) / (maxLength * maxLength);
  return textScore;
};

// if distance > 500 km: this score is 0
function getDistanceScore (distance) {
  return -(1/Math.pow(500000, 0.5))*Math.pow(distance, 0.5) + 1;
}

module.exports = {
  getText: getLevenhSteinBasedScore,
  getDistance: getDistanceScore,
  getCombined: function (textScore, distanceScore) {
    
    return (textScore * textScoreWeight) + (distanceScore * distanceScoreWeight);
  }
};