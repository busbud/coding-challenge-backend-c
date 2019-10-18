const url = require('url');
const querystring = require('querystring');
const deburr = require('lodash.deburr');
const scoreCalculators = require('./scoreCalculators');
const EmptySearchError = require('./errors/emptySearchError');

module.exports = { handleSuggestions };

// searchCities should be a function in the form of (searchText) => (Promise<CityData[]> | CityData[])
async function handleSuggestions(searchCities, req, res) {
  try {
    const queryStrings = querystring.parse(url.parse(req.url).query);
    const suggestions = (await getSuggestions(searchCities, queryStrings))
      .sort((cityA, cityB) => cityB.score - cityA.score)
  
    const responseCode = suggestions.length > 0 ? 200 : 404;
    res.writeHead(responseCode, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      suggestions
    }));
  } catch (error) {
    if (error instanceof EmptySearchError) {
      console.error(`Error processing request ${req.url}, q queryparam should not be empty`);
      res.writeHead(422, 'the q query parameter should not be empty', { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({
        suggestions: []
      }));
    } else {
      throw error;
    }
  }
}

async function getSuggestions(searchCities, queryStrings) {
  if (!queryStrings.q) {
    throw new EmptySearchError();
  }

  const cities = await searchCities(queryStrings.q).slice(0, 50);
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