function getScore(location, searchValue, options = {}) {
  const { latitude, longitude } = options;
  let [city] = location?.name?.split(',');
  city = city?.toLowerCase();

  if (city.indexOf(searchValue) !== 0) {
    return 0;
  }

  let score = 0;
  let longer = city;
  let shorter = searchValue;
  if (city.length < searchValue.length) {
    longer = searchValue;
    shorter = city;
  }
  let longerLength = longer.length;
  if (longerLength === 0) {
    score  = 1.0;
  } else {
    score = (longerLength - calcDistance(longer, shorter)) / parseFloat(longerLength);
  }

  if (latitude && longitude) {
    let latScore = 1 - (Math.abs(latitude - location?.latitude) * 111) / 40075;
    let longScore = 1 - (Math.abs(longitude - location?.longitude) * 111) / 40075;
    const totalScore = score / 3 + latScore / 3 + longScore / 3;
    return totalScore;
  }

  return score;
}

function calcDistance(s1, s2) {
  var costs = new Array();
  for (var i = 0; i <= s1.length; i++) {
    var lastValue = i;
    for (var j = 0; j <= s2.length; j++) {
      if (i === 0) {
        costs[j] = j;
      } else {
        if (j > 0) {
          var newValue = costs[j - 1];
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