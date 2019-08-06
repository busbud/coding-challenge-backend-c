var http = require('http');
var port = process.env.PORT || 2345;
const querystring = require('querystring');
const url = require('url');
const fs = require('fs');



fs.readFile('./data/cities_canada-usa.tsv', function (err, data) {
    if (err) {
        throw err;
    }
    const content = data.toString();
    processFile(content);         
});

let cities;
function processFile(content) {
  cities = content.split('\n').map(city => {
    const info = city.split('\t');
    return {
      id: info[0],
      name: info[1],
      // asciiname: info[2],
      // alternatenames: info[3],
      latitude: info[4],
      longitude: info[5],
      // featureClass: info[6],
      // featureCode: info[7],
      countryCode: info[8],
      // cc2: info[9],
      admin1: info[10],
      // admin2: info[11],
      // admin3: info[12],
      // admin4: info[13],
      population: info[14],
      // elevation: info[15],
      // dem: info[16],
      // timezone: info[17],
      // modDate: info[18],
    };
  });
  return cities;
}

function filter(cities, query) {
  const filteredCities = cities.filter((city) => (city.population > 5000) && (city.countryCode == "US" || "CA") && (city.name.toLowerCase().includes(query.q.toLowerCase())))
  for (i = 0; i < filteredCities.length; i++) {
    filteredCities[i].score = 0.1;
    if (query.latitude) {
      if (parseFloat(query.latitude) - parseFloat(filteredCities[i].latitude) < 1) {
        filteredCities[i].score += 0.1;
      }
    }
  }
  const sortedSuggestions = filteredCities.sort((a,b) => (b.score - a.score))
  return sortedSuggestions;
}

module.exports = http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    const parsedUrl = url.parse(req.url, true);
    const query = parsedUrl.query;
    res.end(JSON.stringify({
      suggestions: filter(cities, query),
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);