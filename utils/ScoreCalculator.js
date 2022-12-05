// These functions are related to calculating the score
// I'm using the levenshtein distance algorithm to compare strings
// Obviously I didn't know the levenshtein distance alogrithm prior to this assessment
// and I'm not gonna pretend like I came up with this on my own
// the majority of this function was found on stack overflow
// and I manipulated the code to work how I wanted it to

function getScore(location, searchValue, options = {}) {
  const { latitude, longitude } = options;
  let [city] = location?.name?.split(',');
  city = city?.toLowerCase();

  // return 0 if searchValue isn't at the beggining of the string
  if (city.indexOf(searchValue) !== 0) {
    return 0;
  }

  // compute levenshtein distance score
  let score = 0;
  let longer = city;
  let shorter = searchValue;
  if (city.length < searchValue.length) {
    longer = searchValue;
    shorter = city;
  }
  let longerLength = longer.length;
  if (longerLength === 0) {
    score = 1.0;
  } else {
    score = (longerLength - calcDistance(longer, shorter)) / parseFloat(longerLength);
  }

  // if latitude & longitude have been passed in the options param we can use them to boost the score
  if (latitude && longitude) {
    // here I found that the distance in Km between latitude is 111 (longitude is 111.321, close enough to 111)
    // then the circumference of the world is 40075Km
    // so I can compute a relative score of how far away the suggestion object's lat & long is compared to the lat & long in the query
    const latScore = 1 - (Math.abs(latitude - location?.latitude) * 111) / 40075;
    const longScore = 1 - (Math.abs(longitude - location?.longitude) * 111) / 40075;
    // We can now update the score by dividing all scores by 3 and adding them up
    // We divide by 3 to make the scores have the same weight
    const totalScore = score / 3 + latScore / 3 + longScore / 3;
    return totalScore;
  }

  // if lat & long was not passed in the options then just return the score
  return score;
}

// compute the distance of string comparision
// this will calc how close one string is to another
function calcDistance(s1, s2) {
  let costs = new Array();
  for (let i = 0; i <= s1.length; i++) {
    let lastValue = i;
    for (let j = 0; j <= s2.length; j++) {
      if (i === 0) {
        costs[j] = j;
      } else {
        if (j > 0) {
          let newValue = costs[j - 1];
          if (s1[i - 1] != s2[j - 1]) {
            newValue = Math.min(Math.min(newValue, lastValue), costs[j]) + 1;
          }
          costs[j - 1] = lastValue;
          lastValue = newValue;
        }
      }
    }
    if (i > 0) {
      costs[s2.length] = lastValue;
    }
  }
  return costs[s2.length];
}

module.exports = { getScore };