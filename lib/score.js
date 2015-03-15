
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
  // not bad...
  var textScore =  (str1.length / str2.length) * ((maxLength - levenshteinDistance ) * (maxLength - levenshteinDistance )) / (maxLength * maxLength);

  // try 2
  // var diffChars = Math.max(str1.length - str2.length, 0);
  // var textScore = 
  return textScore;
};

function getDistanceScore (distance) {
  return 1;
}

module.exports = {
  getText: getLevenhSteinBasedScore,
  getDistance: getDistanceScore,
  getCombined: function (textScore, distanceScore) {
    return (textScore * textScoreWeight + distanceScore * distanceScoreWeight)
  }
};