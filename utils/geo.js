const distance = require("@turf/distance").default;
const { point } = require("@turf/helpers");

/**
 * is the coordinate object valid
 * @param {latitude:  string|number, longitude: string|number} coord
 */
const numericalCoordinate = coord => {
  const latitude = parseFloat(coord.latitude);
  const longitude = parseFloat(coord.longitude);
  return [latitude, longitude].every(feat => !isNaN(feat))
    ? { latitude, longitude }
    : null;
};

/**
 * compute distance in km for two points
 * @param {latitude:  string|number, longitude: string|number} cityA
 * @param {latitude: string|number, longitude: string|number} cityB
 */
function distanceBetween(cityA, cityB) {
  // if inputs are not valid; return -1
  const cityANumerical = numericalCoordinate(cityA);
  const cityBNumerical = numericalCoordinate(cityB);

  if ([cityANumerical, cityBNumerical].some(coord => !coord)) return -1;
  return distance(
    point([cityANumerical.latitude, cityANumerical.longitude]),
    point([cityBNumerical.latitude, cityBNumerical.longitude])
  );
}

/** sort cities list by ascending distance */
function sortByDistance(pivot, cities = []) {
  return cities.sort((a, b) => {
    return distanceBetween(pivot, a) - distanceBetween(pivot, b);
  });
}

module.exports = {
  numericalCoordinate,
  sortByDistance,
  distanceBetween
};
