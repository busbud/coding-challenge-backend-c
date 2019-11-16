const config = require('./global.js');
/**
 * Uses Google Maps libraries to get distances in km
 * to get the distance, I use default mode is 'driving' due to mode: 'transit' sometimes do not return a distance because
 * there is not an existing route to cover in bus
 // mode: 'transit',//default mode is 'driving'
 // transit_mode: ['bus', 'rail'],
 // transit_routing_preference: 'fewer_transfers',
 * @param [String] string
 * @param [String] string
 *
 * @returns
 */
module.exports.getDistances = (origin, arrayMatchingDestinations) => {
  return new Promise((resolve, reject) => {
    const googleMapsClient = require('@google/maps').createClient({
      key: config.GOOGLE_MAPS_KEY,
      Promise: Promise, // 'Promise' is the native constructor.
    });

    let originsArrayGoogleFormat = [];
    origin.latitude & origin.longitude
      ? originsArrayGoogleFormat.push(`${origin.latitude},${origin.longitude}`)
      : originsArrayGoogleFormat.push(arrayMatchingDestinations[0].name);

    let destinationsArrayGoogleFormat = arrayMatchingDestinations.map(
      (d) => `${d.latitude},${d.longitude}`
    );

    googleMapsClient
      .distanceMatrix({
        origins: originsArrayGoogleFormat,
        destinations: destinationsArrayGoogleFormat,
      })
      .asPromise()
      .then((response) => {
        let elementsArray = response.json.rows[0].elements; //console.log(objRow);
        elementsArray.forEach((objRow, i) => {
          let { distance, duration, status } = objRow;
          arrayMatchingDestinations[i].distance = distance.value; //distance in m
          arrayMatchingDestinations[i].duration = duration.value; //time
          arrayMatchingDestinations[i].status = status;
        });
        resolve(arrayMatchingDestinations);
      })
      .catch((err) => {
        reject(err);
      });
  });
};
