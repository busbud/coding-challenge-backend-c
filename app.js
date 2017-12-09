var http = require('http');
var url = require("url");
var suggest = require('./suggest')
var port = process.env.PORT || 2345;

var csv = require("fast-csv");

let cities = []

// We read the CSV, to be able to do the search
// so we wait the end before we start the server
let promise = new Promise((resolve, reject) => {

  const httpServer = http.createServer(function (req, res) {

    if(req.url.indexOf('/hello') === 0) {
      res.writeHead(200, { 'Content-Type': 'text/plain' });
      res.end('Hello CI and CD :)')
    }else if (req.url.indexOf('/suggestions') === 0) {
      var parsedUrl = url.parse(req.url, true); // true to get query as object
      var queryAsObject = parsedUrl.query;
      console.log('QUERY', queryAsObject)

      if (!queryAsObject.q || typeof queryAsObject.q != 'string' ||
          queryAsObject.q.length < 3) {
        res.writeHead(400, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          error: 'q parameter is required and has to have at least 3 chars'
        }));
        res.end()
        return;
      }

      let qLatitude = parseFloat(queryAsObject.latitude)
      let qLongitude = parseFloat(queryAsObject.longitude)
      let latitude = undefined
      let longitude = undefined
      if (qLatitude > -90 && qLatitude < 90) {
        latitude = qLatitude
      }
      if (qLongitude > -180 && qLongitude < 180) {
        longitude = qLongitude
      }

      let matchedCities = suggest(cities, queryAsObject.q, latitude, longitude)
      
      if (matchedCities.length === 0) {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: []
        }));
      } else {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: matchedCities
        }));
      }
    } else {
      res.end();
    }
  })

  csv
    .fromPath("data/cities_canada-usa.tsv", { delimiter: '\t', quote: null })
    .on("data", function ([
      geonameid,
      name,
      asciiname,
      alternatenames,
      latitude,
      longitude,
      featureClass,
      featureCode,
      countryCode,
      countryCode2,
      adminCode1
    ]) {
      let city = {
        asciiname,
        name,
        alternatenames,
        latitude,
        longitude,
        countryCode,
        adminCode1
      }
      cities.push(city)
      console.log(city);
    })
    .on("end", function () {
      console.log("done");
      console.log('Server running at http://127.0.0.1:%d/suggestions', port);
      httpServer.listen(port, '0.0.0.0');
      resolve(httpServer)
    });

})



module.exports = promise;

