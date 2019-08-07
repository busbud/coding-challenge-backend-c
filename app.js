var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs');



fs.readFile('./data/cities_canada-usa.tsv', 'utf8', function (err, data) {
    if (err) {
        throw err;
    }
    const content = data.toString();
    processFile(content);         
});

let cities = [];
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

  for (i=0; i<cities.length; i++) {
   switch(cities[i].admin1){
   case "01":
     cities[i].name = `${cities[i].name}, Alberta`
     break;
    case "10":
      cities[i].name = `${cities[i].name}, Quebec`
      break;
  }}
  return cities;
}
//http://download.geonames.org/export/dump/admin1CodesASCII.txt
// CA.02	British Columbia	British Columbia	5909050
// CA.03	Manitoba	Manitoba	6065171
// CA.04	New Brunswick	New Brunswick	6087430
// CA.13	Northwest Territories	Northwest Territories	6091069
// CA.07	Nova Scotia	Nova Scotia	6091530
// CA.14	Nunavut	Nunavut	6091732
// CA.08	Ontario	Ontario	6093943
// CA.09	Prince Edward Island	Prince Edward Island	6113358
// CA.10	Quebec	Quebec	6115047
// CA.11	Saskatchewan	Saskatchewan	6141242
// CA.12	Yukon	Yukon	6185811
// CA.05	Newfoundland and Labrador	Newfoundland and Labrador	6354959


function filter(cities, query) {
  const filteredCities = cities.filter((city) => (city.population > 5000) && (city.countryCode == "US" || "CA") && (city.name.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase().includes(query.q.normalize("NFD").replace(/[\u0300-\u036f]/g, "").toLowerCase())))
  for (i = 0; i < filteredCities.length; i++) {
    filteredCities[i].score = 0.1;
    if (query.q.toLowerCase() == filteredCities[i].name.toLowerCase()) {
      filteredCities[i].score = 0.8;
    }
    if (query.latitude) {
      if (parseFloat(query.latitude) - parseFloat(filteredCities[i].latitude) < 1) {
        filteredCities[i].score += 0.1;
      }
    }
    if (query.longitude) {
      if (parseFloat(query.longitude) - parseFloat(filteredCities[i].longitude) < 1) {
        filteredCities[i].score += 0.1;
      }
    }
  }
  const sortedSuggestions = filteredCities.sort((a,b) => (b.score - a.score));
  console.log(sortedSuggestions);
  return sortedSuggestions;
}

module.exports = http.createServer(function (req, res) {
  // res.writeHead(200, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    const parsedUrl = url.parse(req.url, true);
    const query = parsedUrl.query;
    const suggestions = filter(cities, query);
    if (suggestions.length < 1) {
      res.status = 404;
      res.statuscode = 404;
      res.end(JSON.stringify({
        suggestions: suggestions,
      }));
    } else {
      res.statusCode = 200;
      res.end(JSON.stringify({
        suggestions: suggestions,
      }));
    }
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);