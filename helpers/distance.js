/**
 * Meassure the distance between two sets of coordinates.
 *
 * @param {*} lng1
 * @param {*} lat1
 * @param {*} lng2
 * @param {*} lat2
 *
 * @return {float} distance
 */
module.exports = (lng1, lat1, lng2, lat2) => {
  if ((lat1 == lat2) && (lng1 == lng2)) {
    return 0;
  } else {
    const radlat1 = Math.PI * lat1/180;
    const radlat2 = Math.PI * lat2/180;
    const theta = lng1 - lng2;
    const radtheta = Math.PI * theta/180;
    let distance = Math.sin(radlat1) * Math.sin(radlat2) + Math.cos(radlat1) * Math.cos(radlat2) * Math.cos(radtheta);
    if (distance > 1) {
      distance = 1;
    }
    distance = Math.acos(distance);
    distance = distance * 180/Math.PI;
    distance = distance * 60 * 1.1515 * 1.609344;
    return distance;
  }
};
