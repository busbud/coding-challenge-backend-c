/**
 * Calculte distance between two points with pythagoras
 * @param {number} lat1 First latitude point
 * @param {number} lon1 First longitude point
 * @param {number} lat2 Second latitude point
 * @param {number} lon2 Second longitude point
 */
function lineDist(lat1, lon1, lat2, lon2) {
  const a = lat1 - lat2;
  const b = lon1 - lon2;

  return Math.sqrt(a * a + b * b);
}

/**
 * Get the string distance and coordinate distance
 * @param {string} a First term
 * @param {string} b Second term
 * @param {number} lat Latiude to search
 * @param {number} long Longitude to search
 * @param {number} cityLat Latitude of the city
 * @param {number} cityLong Longitude of the city
 */
module.exports.get = (a, b, lat, long, cityLat, cityLong) => {
  const min = Math.min(a.length, b.length);
  let x = 0; // number of identic consecutive characters

  // If term is greater than city name, return null score
  if (a.length > b.length) {
    return 0;
  }

  for (let i = 1; i < min; i += 1) {
    if (a[i] === b[i] && a[i - 1] === b[i - 1]) {
      x += 1;
    }
  }

  let ratio = (x / Math.max(a.length, b.length));

  // Add longitude and latitude distance to the current ratio
  if (!isNaN(lat) && !isNaN(long)) {
    const r = lineDist(lat, long, cityLat, cityLong);
    ratio = ((ratio) + (1 - (r / 100))) / 2;
  }

  return ratio;
};
