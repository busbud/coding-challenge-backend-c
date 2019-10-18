var http = require('http');
var port = process.env.PORT || 2345;
const { readCityData } = require('./readCityData');
const { getCitiesSearcher } = require('./searchCities');
const { handleSuggestions } = require('./suggestionsHandler');

const searchCitiesPromise = getFilteredCityData().then(citiesData => getCitiesSearcher(citiesData));

async function getFilteredCityData() {
  const citiesData = await readCityData('./data/cities_canada-usa.tsv');
  return citiesData.filter((cityData) =>
    (cityData.country === 'CA' || cityData.country === 'US') &&
    Number(cityData.population) > 5000);
}

module.exports = http.createServer(async function (req, res) {
  res.writeHead(404, { 'Content-Type': 'text/plain' });

  if (req.url.includes('/suggestions')) {
    const searchCities = await searchCitiesPromise;
    handleSuggestions(searchCities, req, res);
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
