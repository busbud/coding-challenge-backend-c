var _ = require("lodash");

exports.formatRawCity = function(rawCity){
  // The file contains columns title and empty lines, we need to remove them.
  if (rawCity[1] == 'name' || _.isEmpty(rawCity[1])) {
    return false;
  }
  else{
    // Name will be replaced later by formatedName, and population will be removed. But for now, we need to get them because they will be useful really soon.
    var city = {
      name: rawCity[1],
      formatedName: rawCity[1] + ', ' + rawCity[10] + ', ' + rawCity[8],
      latitude: rawCity[4],
      longitude: rawCity[5],
      population: rawCity[14]
    };

    // alt will also be removed. It contains all alternative city names.
    if (!_.isEmpty(rawCity[3])) {
      city.alt = rawCity[3].split(",");
    }

    return city;
  }
};

exports.makeSelection = function(allCities, query){
  var selectedCities = [];

  _.each(allCities, function(city) {
    // We select only cities where population is > 5000 (Currently, in the TSV file, all cities are > 5000).
    if (parseInt(city.population, 10) > 5000) {

      // We get an alternate city name which could match the query if it exists.
      var altName = _.find(city.alt, function(item){
        if (item.indexOf(query.cityName) === 0) {
          return item;
        }
      });

      // We check if either the main city name matches the query, either the alternate city exist (so matches the query).
      if (city.name.indexOf(query.cityName) === 0 || !_.isUndefined(altName) ) {
        // We get the score.
        city.score = calcScore(city, query, altName);
        selectedCities.push(city);
        // We replace the name (Ex: London) by the formatedName (Ex: London, OH, USA)
        city.name = city.formatedName;
        // We delete all useless informations which won't be printed in the response JSON.
        delete city.formatedName;
        delete city.alt;
        delete city.population;
      }
    }
  });

  // We sort the selected cities by their score, ascending...
  selectedCities = _.sortBy(selectedCities, function(item){
    return item.score;
  });

  // ...no, finally descending.
  return selectedCities.reverse();
};

function calcScore(city, query, altName){
  // We start the score at 1 and it will possibly be substracted multiple time through the process.
  var score = 1.00;
  var cityName = city.name;
  if (!_.isUndefined(altName)) {
    cityName = altName;
  }

  var diffNbChar = cityName.length - query.cityName.length;
  // The score loses 0.05 by the difference of characters between the query city name and the true city name (or alternative city name).
  score -= 0.05 * diffNbChar;

  _.each(['latitude', 'longitude'], function(coord){
    if (!_.isNaN(query[coord])) {
      var diffCoord = Math.abs(city[coord] - query[coord]);
      // The score lose 0.02 by the difference between the query latitude/longitude and the true latitude/longitude.
      score = truncate(score - diffCoord/50, 2);
    }
  });

  if (score < 0.01) {
    score = 0.01;
  }
  return score.toFixed(2);
}

// Useful to truncate a number after any digits after decimal point.
function truncate(num, digits) {
  var numS = num.toString(),
      decPos = numS.indexOf('.'),
      substrLength = decPos == -1 ? numS.length : 1 + decPos + digits,
      trimmedResult = numS.substr(0, substrLength),
      finalResult = isNaN(trimmedResult) ? 0 : trimmedResult;

  return parseFloat(finalResult);
}