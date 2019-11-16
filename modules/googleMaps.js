const config = require('./global.js');
/**
 * Uses Google Maps libraries to get distances in km
 * to get the distance, I use default mode is 'driving' due to mode: 'transit' sometimes do not return a distance because
 * there is not an existing route to cover in bus
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

    // let originsArrayGoogleFormat = ['Hornsby Station, NSW', 'Chatswood Station, NSW'];
    // let destinationsArrayGoogleFormat = ['Central Station, NSW', 'Parramatta Station, NSW'];
    // let originsArrayGoogleFormat = ['Terminus Longueuil, Longueuil, QC'];
    // let destinationsArrayGoogleFormat = ['517 rue varennes, Longueuil, QC'];
    // originsArrayGoogleFormat = ['43.70011,-79.4163']; //"295 Forest Hill Rd, Toronto, ON M5P 2N7, Canada"
    // destinationsArrayGoogleFormat = [
    //   'London, ON, Canada',
    //   'London, OH, USA',
    //   'London, KY, USA',
    //   'Londontowne, MD, USA',
    // ];
    // let destinationsArrayGoogleFormat = ['London, ON, Canada'];//[{"distance":{"text":"190 km","value":190000},"duration":{"text":"2 hours 7 mins","value":7622},"status":"OK"}]}]
    // let destinationsArrayGoogleFormat = ['London, OH, USA'];//[{"elements":[{"distance":{"text":"741 km","value":740523},"duration":{"text":"7 hours 7 mins","value":25629},"status":"OK"}]}]
    // let destinationsArrayGoogleFormat = ['London, KY, USA'];//[{"elements":[{"distance":{"text":"1,037 km","value":1037114},"duration":{"text":"9 hours 56 mins","value":35734},"status":"OK"}]}]
    // let destinationsArrayGoogleFormat = ['Londontowne, MD, USA'];//[{"elements":[{"distance":{"text":"802 km","value":801892},"duration":{"text":"8 hours 31 mins","value":30673},"status":"OK"}]}]

    googleMapsClient
      .distanceMatrix({
        origins: originsArrayGoogleFormat,
        destinations: destinationsArrayGoogleFormat,
        // mode: 'transit',//default mode is 'driving'
        // transit_mode: ['bus', 'rail'],
        // transit_routing_preference: 'fewer_transfers',
      })
      .asPromise()
      .then((response) => {
        //console.log(response.json);
        let elementsArray = response.json.rows[0].elements; //console.log(objRow);
        elementsArray.forEach((objRow, i) => {
          let { distance, duration, status } = objRow;
          arrayMatchingDestinations[i].distance = distance.value; //distance in m
          arrayMatchingDestinations[i].duration = duration.value; //time
          arrayMatchingDestinations[i].status = status;
        });
        // arrayMatchingDestinations.map((d) =>
        //   console.log(`${d.name} - ${d.distance} km`)
        // );
        resolve(arrayMatchingDestinations);
      })
      .catch((err) => {
        reject(err);
      });
  });
};
