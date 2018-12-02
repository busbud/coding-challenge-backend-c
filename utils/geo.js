const distance = require("@turf/distance").default;
const { point } = require("@turf/helpers");
/**
 *
 * @param {latitude: number, longitude: number} cityA
 * @param {latitude: number, longitude: number} cityB
 */
function distanceBetween(cityA, cityB) {
  const cityALat = parseFloat(cityA.latitude);
  const cityALon = parseFloat(cityA.longitude);
  const cityBLat = parseFloat(cityB.latitude);
  const cityBLon = parseFloat(cityB.longitude);

  // not valid data as inputs
  if ([cityALat, cityALon, cityBLat, cityBLon].some(value => isNaN(value)))
    return -1;

  return distance(point([cityALat, cityALon]), point([cityBLat, cityBLon]));
}

/** sort cities list by ascending distance */
function sortByDistance(pivot, cities = []) {
  return cities.sort((a, b) => {
    return distanceBetween(pivot, a) - distanceBetween(pivot, b);
  });
}

module.exports = {
  sortByDistance,
  distanceBetween
};
