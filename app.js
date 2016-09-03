var http = require('http');
var url = require('url');
var parse = require('csv-parse/lib/sync'); // we're gonna use syncronous method... it's slow, but we need the data before proceding anyway
var functions = require('./functions');
var fs = require('fs');
var port = process.env.PORT || 2345;

let citiesFile = fs.readFileSync(__dirname+'/data/cities_canada-usa.tsv');
var cities = parse(
  citiesFile,
  {
    delimiter: "\t",
    quote: "",
    columns: true
  });

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    let params = url.parse(req.url, true).query
      , query = params.q
      , latitude = params.latitude || 0
      , longitude = params.longitude || 0
      , results = [];

    cities.forEach((city) => {
      var regex = new RegExp(query, 'i');

      if (regex.test(city.name) || regex.test(city.alt_name)) {
        results.push({
          name: functions.getCityNameMatch(query, city.name, city.alt_name)+', '+functions.getStateSymbol(city)+', '+functions.getCountryName(city.country),
          latitude: city.lat,
          longitude: city.long,
          score: functions.calculateScore(query, latitude, longitude, city)
        });
      }
    });

    results.sort((city1, city2) => {
      if (city2.score < city1.score) return -1;
      else if (city2.score > city1.score) return +1;
      else {
        if (city2.name < city1.name || city1.name.indexOf(query) === 1) return -1;
        else if (city2.name > city1.name) return +1;
        else return 0;
      }
    });

    res.writeHead(results.length < 1 ? 404 : 200, {'Content-Type': 'text/json'});

    res.end(JSON.stringify({
      suggestions: results
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
