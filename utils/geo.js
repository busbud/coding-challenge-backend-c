const distance = require("@turf/distance");
/**
 *
 * @param {lat: number, lon: number} cityA
 * @param {lat: number, lon: number} cityB
 */
export function distanceBetween(cityA, cityB) {
  const cityALat = parseFloat(cityA.lat);
  const cityALon = parseFloat(cityA.lon);
  const cityBLat = parseFloat(cityB.lat);
  const cityBLon = parseFloat(cityB.lon);

  if ([cityA, cityALon, cityBLat, cityBLon].some(value => value === undefined))
    //throw new Error("LAT_LON_ARE_NOT_FLOATS");
    return -1;

  return distance(
    { lat: cityALat, lon: cityALon },
    { lat: cityBLat, lon: cityBLon }
  );
}

/** sort cities list by ascending distance */
export function sortByDistance(cities = []) {
  return cities.sort(distanceBetween);
}
