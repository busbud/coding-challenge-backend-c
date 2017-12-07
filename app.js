var http = require('http');
var url = require("url");
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

      let matchedCities = cities.filter(city => city.name.indexOf(queryAsObject.q) !== -1)
      
      if (matchedCities.length === 0) {
        res.writeHead(404, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: []
        }));
      } else {
        res.writeHead(200, { 'Content-Type': 'text/plain' });
        res.end(JSON.stringify({
          suggestions: matchedCities.map(city => Object.assign(city, {score: 1}))
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
      countryCode
    ]) {
      let city = {
        name: asciiname,
        latitude,
        longitude
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

