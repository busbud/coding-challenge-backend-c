var http = require('http');
var url = require('url');
var parse = require('csv-parse/lib/sync'); // we're gonna use syncronous method... it's slow, but we need the data before proceding anyway
var _ = require('underscore');
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


function getCityNameMatch(query, name, alt_names) {
  let result = "";
  let names = alt_names.split(',');
  names.unshift(name);

  if (names.indexOf(query) !== -1) result = query;
  else {
    names.forEach((name) => {
      if (name.indexOf(query) !== -1) result = name;
    });
  }

  return result;
}

function getStateSymbol(city) {
  if (city.country === "US") return city.admin1;
  else {
    let map = {
      '01': "AB",
      '02': "BC",
      '03': "MB",
      '04': "NB",
      '05': "NL",
      '07': "NS",
      '08': "ON",
      '09': "PE",
      '10': "QC",
      '11': "SK",
      '12': "YT",
      '13': "NT"
    };

    return map[city.admin1];
  }
}

function getCountryName(countryCode) {
  let map = {
    'CA': "Canada",
    'US': "USA"
  };

  return map[countryCode];
}

module.exports = http.createServer(function (req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    let params = url.parse(req.url, true).query
      , query = params.q
      , latitude = params.latitude || 0
      , longitude = params.longitude || 0
      , results = [];

    cities.forEach((city) => {
      if (city.name.indexOf(query) !== -1 || city.alt_name.indexOf(query) !== -1) {
        results.push({
          name: getCityNameMatch(query, city.name, city.alt_name)+', '+getStateSymbol(city)+', '+getCountryName(city.country),
          latitude: city.lat,
          longitude: city.long,
          score: 0.5 // temp
        });
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
