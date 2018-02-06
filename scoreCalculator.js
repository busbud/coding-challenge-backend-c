var geodist = require('geodist');
require("string_score");

let calculateScore = (data, queryParam, latitude, longitude) => {
  let minMaxDistance;
  if (latitude && longitude) {
    minMaxDistance = findMaxMinDistance(data, latitude, longitude); //finds max and min distance
  }
  return findScore(data, queryParam, latitude, longitude, minMaxDistance);
}

let findMaxMinDistance = (data, latitude, longitude) => {
  let minMaxDistance = {};
  for (let i = 0; i < data.length; i++) {
    let cityInfo = data[i].value;
    const distance = geodist({
      lat: cityInfo.latitude,
      lon: cityInfo.longitude
    }, {
      lat: latitude,
      lon: longitude
    })
    if (!minMaxDistance.maxDistance || minMaxDistance.maxDistance < distance) {
      minMaxDistance.maxDistance = distance;
    }
    if (!minMaxDistance.minDistance || minMaxDistance.minDistance > distance) {
      minMaxDistance.minDistance = distance;
    }
  }
  return minMaxDistance;
}

let findScore = (data, queryParam, latitude, longitude, minMaxDistance) => {
  let result = [];
  for (let i = 0; i < data.length; i++) {
    const cityInfo = data[i].value;
    let stringScore = findStringScore(cityInfo, queryParam); //find score based on text
    let distanceScore;
    let score;
    if (minMaxDistance) { // only if thrs latitude and longitude
      distanceScore = findDistanceScore(cityInfo, latitude, longitude, minMaxDistance); //find score based on distance
      score = Math.round(((stringScore + distanceScore) / 2) * 10) / 10;
    } else {
      score = Math.round(stringScore * 10) / 10;
    }

    cityInfo.score = score;
    result.push(cityInfo);
  }
  return result.sort(function(obj1, obj2) {
    return obj2.score - obj1.score; //sort descending based on score
  });
}

// Math.round(cityInfo.name.score(queryParam) * 10) / 10;
let findStringScore = (cityInfo, queryParam) => {
  return cityInfo.name.score(queryParam);
}

let findDistanceScore = (cityInfo, latitude, longitude, minMaxDistance) => {
  let distance = geodist({
    lat: cityInfo.latitude,
    lon: cityInfo.longitude
  }, {
    lat: latitude,
    lon: longitude
  });
  let score = 1 - (distance - minMaxDistance.minDistance) / (minMaxDistance.maxDistance - minMaxDistance.minDistance);
  return score;
}

exports.calculateScore = calculateScore;