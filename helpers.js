// Unfortunately we cannot use the browser package navigator
// since requests aren't necessarily called from a browser.
const publicIp = require('public-ip');
const geolib   = require('geolib');
const geoip    = require('geo-from-ip');

const getUserLL = async () => {
  const ip = await publicIp.v4();
  const user = geoip.allData(ip);
  if (user.error) { // If the IP address cannot be located
    return;
  } else {
    const userLat = user.location.latitude
    const userLng = user.location.longitude
    return {userLat, userLng}
  }
}

const getScore = function(city, lat, lng) {
  // We will inverse the score at the end to achieve a final interval of 0 to 1
  // If no lat and lng are supplied to the function then only population determines score
  let score = city.population;

  let distance = 1; // Initialize as 1 because we divide by the sqrt(distance)
  if (lat && lng) {
    distance = geolib.getDistance(
      {latitude: city.latitude, longitude: city.longitude},
      {latitude: lat, longitude: lng}
    );
    // Taking the square root means that past a certain distance population
    // is what we really care about. This is a heuristic approach
    score = score / Math.sqrt(distance);
  }

  // Will squish the score into the range 0 to 1
  score = Math.tanh(score / 1000);
  return score;
}

module.exports = {getUserLL, getScore}
