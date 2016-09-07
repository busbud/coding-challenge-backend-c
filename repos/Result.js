var functions = require('../functions');

var ResultModel = function(query, city) {
  return {
    name: functions.composeFullName(query, city),
    latitude: city.latitude,
    longitude: city.longitude,
    score: functions.calculateScore(query, city)
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
