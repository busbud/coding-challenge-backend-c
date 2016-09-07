module.exports = {
  composeFullName: composeFullName,
  getCityNameMatch: getCityNameMatch,
  getStateSymbol: getStateSymbol,
  getCountryName: getCountryName,
  calculateScore: calculateScore
};

function getCityNameMatch(query, city) {
  let result = "";
  let names = city.alt_names;
  names.unshift(city.name);

  if (names.indexOf(query.search) !== -1) result = query.search;
  else {
    names.forEach((name) => {
      if ((new RegExp(query.search, 'i')).test(name)) result = name;
    });
  }

  return result;
}

function getStateSymbol(city) {
  if (city.country === "US") return city.admin1;
  else {
    let map = {
      '01': "AB",
      '02': "BC",
      '03': "MB",
      '04': "NB",
      '05': "NL",
      '07': "NS",
      '08': "ON",
      '09': "PE",
      '10': "QC",
      '11': "SK",
      '12': "YT",
      '13': "NT"
    };

    return map[city.admin1];
  }
}

function getCountryName(city) {
  let map = {
    'CA': "Canada",
    'US': "USA"
  };

  return map[city.country];
}

function composeFullName(query, city) {
  return getCityNameMatch(query, city)+', '+city.state+', '+city.country
}

function calculateScore(query, match) {
  var score = 0.0;
  var nameMatch = getCityNameMatch(query, match);

  if (query.search === nameMatch) return 1.0;
  else {
    score = query.search.length / nameMatch.length;
    if (nameMatch.indexOf(query.search) !== 0) {
      score *= score;
    }
  }

  if (query.latitude && query.longitude) {
    let distance = Math.sqrt(Math.pow(match.latitude - query.latitude, 2) + Math.pow(match.longitude - query.longitude, 2));

    if (distance < 1) return 1.0;
    else {
      distanceScore = 1 - (1/distance);

      score = (score + distanceScore) / 2;
    }
  }

  return score;
}
