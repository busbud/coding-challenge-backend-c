var app = require('express')();
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

app.get('/suggestions', function (req, res) {
  let query = req.query.q
    , latitude = req.query.latitude || 0
    , longitude = req.query.longitude || 0
    , results = [];

  if (typeof query === "undefined") {
    res.status(422).json({
      'error': "Missing the query parameter"
    });
  } else {
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

    res.status(results.length < 1 ? 404 : 200).json({
      suggestions: results
    });
  }
});

module.exports = app.listen(port, function() {
  console.log('Server running at http://127.0.0.1:%d/suggestions', port);
});
