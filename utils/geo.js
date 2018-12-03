const distance = require("@turf/distance").default;
const { point } = require("@turf/helpers");

/**
 * is the coordinate object valid
 * @param {latitude:  string|number, longitude: string|number} coord
 */
const isCoordinateObjectValid = coord => {
  const lat = parseFloat(coord.latitude);
  const lon = parseFloat(coord.longitude);
  return [lat, lon].every(feat => !isNaN(feat));
};

/**
 * compute distance in km for two points
 * @param {latitude:  string|number, longitude: string|number} cityA
 * @param {latitude: string|number, longitude: string|number} cityB
 */
function distanceBetween(cityA, cityB) {
  // if inputs are not valid; return -1
  if ([cityA, cityB].some(coord => !isCoordinateObjectValid(coord))) return -1;

  return distance(point([cityALat, cityALon]), point([cityBLat, cityBLon]));
}

/** sort cities list by ascending distance */
function sortByDistance(pivot, cities = []) {
  return cities.sort((a, b) => {
    return distanceBetween(pivot, a) - distanceBetween(pivot, b);
  });
}

module.exports = {
  isCoordinateObjectValid,
  sortByDistance,
  distanceBetween
};
