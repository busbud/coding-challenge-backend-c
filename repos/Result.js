var ResultModel = function(query, city) {
  return {
    name: composeFullName(query, city),
    latitude: city.latitude,
    longitude: city.longitude,
    score: calculateScore(query, city)
  };
};

module.exports = {
  convertFromMatches: convertFromMatches
};

function convertFromMatches(query, matches) {
  let results = [];

  matches.forEach(match => {
    results.push(ResultModel(query, match));
  });

  return sortByScore(query, results);
}

function sortByScore(query, results) {
  return results.sort((city1, city2) => {
    if (city2.score < city1.score) return -1;
    else if (city2.score > city1.score) return +1;
    else {
      if (city2.name < city1.name || city1.name.indexOf(query.search) === 1) return -1;
      else if (city2.name > city1.name) return +1;
      else return 0;
    }
  });
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
