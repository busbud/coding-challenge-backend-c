module.exports = {
  getCityNameMatch: getCityNameMatch,
  getStateSymbol: getStateSymbol,
  getCountryName: getCountryName,
  calculateScore: calculateScore
};

function getCityNameMatch(query, name, alt_names) {
  let result = "";
  let names = alt_names.split(',');
  names.unshift(name);

  if (names.indexOf(query) !== -1) result = query;
  else {
    names.forEach((name) => {
      if (name.indexOf(query) !== -1) result = name;
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

function getCountryName(countryCode) {
  let map = {
    'CA': "Canada",
    'US': "USA"
  };

  return map[countryCode];
}

function calculateScore(query, latitude, longitude, match) {
  var score = 0.0;
  var nameMatch = ""+getCityNameMatch(query, match.name, match.alt_name);

  if (query === nameMatch) return 1.0;
  else {
    score = Math.round(query.length / nameMatch.length * 10) / 10;
    if (nameMatch.indexOf(query) > 0) {
      score *= score;
    }
  }

  if (latitude && longitude) {
    let distance = Math.sqrt(Math.pow(match.lat - latitude, 2) + Math.pow(match.long - longitude, 2));

    if (distance < 1) return 1.0;
    else {
      distanceScore = 1 - (1/distance);

      score = (score + distance) / 2;
    }
  }

  return score;
}
