/*
  LevenshteinDistance is a string comparison algorithm basically to find 
  out how many letters should be changed in a word to get another word.
  For Londo and London words, the distance is 1. 
  So if we want to find out how similar Londo word is to London:
  Londons.length = 6
  Distance = 1
  The similarity = (6-1)/6 = 0.8333333333

  Londo to Londontowne
  Londontowne.length = 11
  Distance = 6
  The similarity = (11-6)/11 = 0.45454545454

  So I decided to take this similarity ratio as my word score
*/

/*
  Probably there's a library out there to score string comparisons (and probably better than my solution)
  But I wanted to do it myself 
*/

const calculateLevenshteinDistance = (a, b) => {
  if (a.length == 0) return b.length;
  if (b.length == 0) return a.length;

  var matrix = [];

  // increment along the first column of each row
  var i;
  for (i = 0; i <= b.length; i++) {
    matrix[i] = [i];
  }

  // increment each column in the first row
  var j;
  for (j = 0; j <= a.length; j++) {
    matrix[0][j] = j;
  }

  // Fill in the rest of the matrix
  for (i = 1; i <= b.length; i++) {
    for (j = 1; j <= a.length; j++) {
      if (b.charAt(i - 1) == a.charAt(j - 1)) {
        matrix[i][j] = matrix[i - 1][j - 1];
      } else {
        matrix[i][j] = Math.min(
          matrix[i - 1][j - 1] + 1, // substitution
          Math.min(
            matrix[i][j - 1] + 1, // insertion
            matrix[i - 1][j] + 1
          )
        ); // deletion
      }
    }
  }

  return matrix[b.length][a.length];
};

const getLevenshteinDistanceScore = (word, filterWord) => {
  const distance = calculateLevenshteinDistance(word, filterWord);
  return (word.length - distance) / word.length;
};

module.exports = getLevenshteinDistanceScore;
