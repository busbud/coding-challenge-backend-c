const geodist = require('geodist');
require('string_score');

// This first version includes a score calculator taken from PR 83.
// I'm probably going to change this to include one of my own here.

const calculateScoreForAllAndSort = (data, queryParam, latitude, longitude) => {
  let minMaxDistance;
  if (latitude && longitude) {
    minMaxDistance = findMaxMinDistance(data, latitude, longitude); // finds max and min distance
  }
  return findScore(data, queryParam, latitude, longitude, minMaxDistance);
};

let findMaxMinDistance = (data, latitude, longitude) => {
  const minMaxDistance = {};
  for (let i = 0; i < data.length; i++) {
    const cityInfo = data[i].value;
    const distance = geodist({
      lat: cityInfo.latitude,
      lon: cityInfo.longitude,
    }, {
      lat: latitude,
      lon: longitude,
    });
    if (!minMaxDistance.maxDistance || minMaxDistance.maxDistance < distance) {
      minMaxDistance.maxDistance = distance;
    }
    if (!minMaxDistance.minDistance || minMaxDistance.minDistance > distance) {
      minMaxDistance.minDistance = distance;
    }
  }
  return minMaxDistance;
};

let findScore = (data, queryParam, latitude, longitude, minMaxDistance) => {
  const result = [];
  for (let i = 0; i < data.length; i++) {
    const cityInfo = data[i].value;
    const stringScore = findStringScore(cityInfo, queryParam); // find score based on text
    let distanceScore;
    let score;
    if (minMaxDistance) { // only if thrs latitude and longitude
      distanceScore = findDistanceScore(cityInfo, latitude, longitude, minMaxDistance); // find score based on distance
      score = Math.round(((stringScore + distanceScore) / 2) * 10) / 10;
    } else {
      score = Math.round(stringScore * 10) / 10;
    }

    cityInfo.score = score;
    result.push(cityInfo);
  }
  return result.sort((obj1, obj2) => obj2.score - obj1.score, // sort descending based on score
  );
};

// Math.round(cityInfo.name.score(queryParam) * 10) / 10;
let findStringScore = (cityInfo, queryParam) => cityInfo.name.score(queryParam);

let findDistanceScore = (cityInfo, latitude, longitude, minMaxDistance) => {
  const distance = geodist({
    lat: cityInfo.latitude,
    lon: cityInfo.longitude,
  }, {
    lat: latitude,
    lon: longitude,
  });
  const score = 1 - (distance - minMaxDistance.minDistance) / (minMaxDistance.maxDistance - minMaxDistance.minDistance);
  return score;
};

exports.calculateScoreForAllAndSort = calculateScoreForAllAndSort;
