// This module contains functions to score a results set against different criteria
// Currenty supports:
//  - Geographical distance (Haversine formula)
//  - Name vs prefix (name score)
//  - Population score (higher the /better)

// Decrease score up to -0.7
// We set the treshold at 2000km meaning anything above this will receive the
// max decrease of 0.7. Then we work progressively down to 15km where no decrease
// is applied.
exports.scoreGeo = function(cities, baseLat, baseLong) {
  var maxPenalty = 0.7;
  cities.forEach(function(city) {
    var dist = haversine(baseLat, baseLong, city.lat, city.long);
    // Only apply a negative score if the distance is above 15
    if(dist > 15) {
      var penalty = (dist * maxPenalty) / 2000;
      // Make sure penalty isn't above 0.7
      penalty = (penalty > maxPenalty) ? maxPenalty : penalty;
      city.score = +(city.score - penalty).toFixed(4);
    }
  });
  return cities;
}

// Basic score based on the given prefix and actual name of the city
// Maximum decrease is -0.10
// 3 or less missing letters doesn't decrease score.
// 10 missing letters gives the max decrease.
exports.scoreName = function(cities, prefix) {
  var maxPenalty = 0.10;
  cities.forEach(function(city) {
    var missingLetters = city.name.length - prefix.length;
    if(missingLetters > 3) {
      var penalty = (missingLetters * maxPenalty) / 10;
      penalty = (penalty > maxPenalty) ? maxPenalty : penalty;
      city.score = +(city.score - penalty).toFixed(4);
    }
  });
  return cities;
}

// Give a slight advantage to cities with a bigger population
// Maximum decrease is -0.10
// No penalty for cities with over 100 000 people
// Max penalty if city has less than 7500 people
// A city with over 500 000 people will get a 0.05 bonus
exports.scorePopulation = function(cities) {
  var maxPenalty = 0.10;
  cities.forEach(function(city) {
    if(city.popu < 100000) {
      var penalty = 0;
      if(city.popu < 7500) {
        penalty = maxPenalty;
      } else {
        penalty = maxPenalty - ((city.popu * maxPenalty) / 100000);
      }
      penalty = (penalty > maxPenalty) ? maxPenalty : penalty;
      city.score = +(city.score - penalty).toFixed(4);
    }
    if(city.popu > 500000) {
      city.score += 0.05;
      city.score = (city.score > 1) ? 1 : +city.score.toFixed(4);
    }
  });
  return cities;
}

// The haversine formula is an equation important in navigation, giving great-circle
// distances between two points on a sphere from their longitudes and latitudes.
//
// Source: http://stackoverflow.com/questions/14560999/using-the-haversine-formula-in-javascript
function haversine(baseLat, baseLong, lat, long) {
  var R = 6371; // km

  var x1 = lat-baseLat;
  var dLat = toRad(x1);
  var x2 = long-baseLong;
  var dLon = toRad(x2);
  var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
          Math.cos(toRad(baseLat)) * Math.cos(toRad(lat)) *
          Math.sin(dLon/2) * Math.sin(dLon/2);
  var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
  var d = R * c;

  return d;
}

function toRad(x) {
   return x * Math.PI / 180;
}
