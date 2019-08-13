function geoDistance(lat1, lon1, lat2, lon2, unit) {
  if ((lat1 === lat2) && (lon1 === lon2)) {
    return 0;
  }

  var radlat1 = Math.PI * lat1 / 180;
  var radlat2 = Math.PI * lat2 / 180;
  var theta = lon1 - lon2;
  var radtheta = Math.PI * theta / 180;
  var dist = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
  if (dist > 1) {
    dist = 1;
  }
  dist = Math.acos(dist);
  dist = dist * 180 / Math.PI;
  dist = dist * 60 * 1.1515;
  if (unit === 'K') { dist *= 1.609344; }
  if (unit === 'N') { dist *= 0.8684; }
  return dist;
}

function calcNameScore(searchName, cityName) {
  return searchName.length / cityName.length;
}


function calcGeoScore(lat1, lon1, lat2, lon2) {
  const dist = this.geoDistance(lat1, lon1, lat2, lon2, 'K');

  if (dist > 7000) {
    return 0;
  }
  if (dist < 0) {
    throw new Error("Distance Can't be negative");
  }

  const scoreTemp = 1 - (1 / 7000) * (dist);
  // const score = Math.round(scoreTemp * 100) / 100;
  return scoreTemp;
}

function calcScore(lat1, lon1, lat2, lon2, searchName, cityName) {
  const geoScore = this.calcGeoScore(lat1, lon1, lat2, lon2);
  const nameScore = this.calcNameScore(searchName, cityName);
  const score = Math.round((geoScore * 0.2 + nameScore * 0.8) * 10) / 10;
  return score;
}


module.exports = {
  geoDistance,
  calcScore,
  calcGeoScore,
  calcNameScore
};
