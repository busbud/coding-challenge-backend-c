const url = require('url');
const querystring = require('querystring');
const deburr = require('lodash.deburr');
const scoreCalculators = require('./scoreCalculators');

module.exports = { handleSuggestions };

function handleSuggestions(searchCities, req, res) {
  const queryStrings = querystring.parse(url.parse(req.url).query);
  const suggestions = getSuggestions(searchCities, queryStrings)
    .sort((cityA, cityB) => cityB.score - cityA.score)

  const responseCode = suggestions.length > 0 ? 200 : 404;
  res.writeHead(responseCode, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    suggestions
  }));
}

function getSuggestions(searchCities, queryStrings) {
  const cities = searchCities(queryStrings.q).slice(0, 50);

  const scoreCalculator = queryStrings.latitude && queryStrings.longitude ?
    scoreCalculators.getDistanceBasedScoreCalculator(queryStrings.latitude, queryStrings.longitude) :
    scoreCalculators.calculateSimpleScore;

  return cities.map(city => getCityPacket(scoreCalculator, city));
}

function getCityPacket(scoreCalculator, city) {
  return {
    name: [city.item.name, city.item.admin1, city.item.country].map(deburr).join(', '),
    latitude: city.item.lat,
    longitude: city.item.long,
    score: scoreCalculator(city)
  };
}