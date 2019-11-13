var http = require('http');
var port = process.env.PORT || 2345;

var url = require('url');
require('dotenv').config();

module.exports = http
  .createServer(function(req, res) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });

    //http://127.0.0.1:2345/suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
    if (req.url.indexOf('/suggestions') === 0) {
      console.log(req.url);
      var url_parts = url.parse(req.url, true);
      var urlQuery = url_parts.query;

      let q = urlQuery.q;
      let latitude = urlQuery.latitude ? urlQuery.latitude : null;
      let longitude = urlQuery.longitude ? urlQuery.longitude : null;

      //console.log(process.env.GOOGLE_MAPS_KEY);
      const googleMapsClient = require('@google/maps').createClient({
        key: process.env.GOOGLE_MAPS_KEY,
        Promise: Promise, // 'Promise' is the native constructor.
      });

      // let originsArray = ['Hornsby Station, NSW', 'Chatswood Station, NSW'];
      // let destinationsArray = ['Central Station, NSW', 'Parramatta Station, NSW'];
      // let originsArray = ['Terminus Longueuil, Longueuil, QC'];
      // let destinationsArray = ['517 rue varennes, Longueuil, QC'];
      let originsArray = ['43.70011,-79.4163']; //"295 Forest Hill Rd, Toronto, ON M5P 2N7, Canada"
      let destinationsArray = [
        'London, QC, Canada',
        'London, OH, USA',
        'London, KY, USA',
        'Londontowne, MD, USA',
      ];
      // let destinationsArray = ['London, QC, Canada'];//[{"distance":{"text":"194 km","value":193732},"duration":{"text":"2 hours 7 mins","value":7622},"status":"OK"}]}]
      // let destinationsArray = ['London, OH, USA'];//[{"elements":[{"distance":{"text":"741 km","value":740523},"duration":{"text":"7 hours 7 mins","value":25629},"status":"OK"}]}]
      // let destinationsArray = ['London, KY, USA'];//[{"elements":[{"distance":{"text":"1,037 km","value":1037114},"duration":{"text":"9 hours 56 mins","value":35734},"status":"OK"}]}]
      // let destinationsArray = ['Londontowne, MD, USA'];//[{"elements":[{"distance":{"text":"802 km","value":801892},"duration":{"text":"8 hours 31 mins","value":30673},"status":"OK"}]}]

      googleMapsClient
        .distanceMatrix({
          origins: originsArray,
          destinations: destinationsArray,
          // mode: 'transit',
          // transit_mode: ['bus', 'rail'],
          // transit_routing_preference: 'fewer_transfers',
        })
        .asPromise()
        .then((response) => {
          console.log('resultado:');
          console.log(response.json);
          let elementsArray = response.json.rows[0].elements; //console.log(objRow);
          elementsArray.forEach((objRow) => {
            let { distance, duration, status } = objRow;
            let distanceInKm = distance.value;
            console.log(distance.text);
          });

          res.end(
            JSON.stringify({
              response, //suggestions: [],
            })
          );
        })
        .catch((err) => {
          console.log(err);
        });
    } else {
      res.end();
    }
  })
  .listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
//  let theAddress = '1600 Amphitheatre Parkway, Mountain View, CA';
//   googleMapsClient
//     .geocode({ address: theAddress })
//     .asPromise()
//     .then((response) => {
//       console.log(response.json.results);
//     })
//     .catch((err) => {
//       console.log(err);
//     });
