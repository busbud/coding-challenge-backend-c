const http = require('http')
const url = require("url")

module.exports = function createServer({cities, suggest}, {port}) {
  const httpServer = http.createServer((req, res) => {
    if (req.url.indexOf('/hello') === 0) {
      res.writeHead(200, { 'Content-Type': 'text/plain' })
      res.end('Hello CI and CD :)')
    } else if(req.url.indexOf('/suggestions') === 0) {
      var parsedUrl = url.parse(req.url, true); // true to get query as object
      var queryAsObject = parsedUrl.query;
      console.log('QUERY', queryAsObject)

      if (!queryAsObject.q || typeof queryAsObject.q != 'string' ||
          queryAsObject.q.length < 3) {
        res.writeHead(400, { 'Content-Type': 'application/json' });
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
        .map(city => ({
          name: [city.name, city.adminCode1, city.countryCode].join(', '),
          latitude: city.latitude,
          longitude: city.longitude,
          score: city.score
        }))
      
      if (matchedCities.length === 0) {
        res.writeHead(404, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({
          suggestions: []
        }));
      } else {
        res.writeHead(200, { 'Content-Type': 'application/json' });
        res.end(JSON.stringify({
          suggestions: matchedCities
        }));
      }
    } else {
      res.writeHead(404, { 'Content-Type': 'text/plain' })
      res.end('Not found')
    }
  })
  httpServer.listen(port, '0.0.0.0')
  console.log(`Listening at :${port}`)

  return httpServer
}