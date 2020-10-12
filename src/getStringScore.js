//string ranking
var scoreService = {
  score: function (string1, string2, offset) {
 
  if (string2.length == 0) {
    return 0.9;
  }
 
  if (string2.length > string1.length) {
    return 0.0;
  }
 
  for (var i = string2.length; i > 0; i--) {
    var sub_string2 = string2.substring(0, i);
    var index = string1.indexOf(sub_string2);
 
    if (index < 0) continue;
    if (index + string2.length > string1.length + offset) continue;
 
    var next_string = string1.substring(index + sub_string2.length);
    var next_string2 = null;
 
    if (i >= string2.length) {
      next_string2 = '';
    } else {
      next_string2 = string2.substring(i);
    }
 
    var remaining_score = scoreService.score(next_string,next_string2, offset + index);
 
    if (remaining_score > 0) {
      var score = string1.length - next_string.length;
 
      if (index != 0) {
        var j = 0;
        var c = string1.charCodeAt(index - 1);
 
        if (c == 32 || c == 9) {
          for (j = (index - 2); j >= 0; j--) {
            c = string1.charCodeAt(j);
            score -= ((c == 32 || c == 9) ? 1 : 0.15);
          }
        } else {
          score -= index;
        }
      }
 
      score += remaining_score * next_string.length;
      score /= string1.length;
 
      return score;
    }
  }
 
  return 0.0;
}}

module.exports = scoreService