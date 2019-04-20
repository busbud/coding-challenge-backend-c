const express = require('express');
const geolib = require('geolib');
const app = express();
const dataUtils = require('./data-utils');
const port = process.env.PORT || 2345;

let citiesData = require('./sync-load-data');
citiesData = dataUtils.makeRegionsReadable(citiesData);
citiesData = dataUtils.renameLatLong(citiesData);

app.get('/suggestions', (req, res) => {
  let suggestions = [];

  if (req.query.q != null && req.query.q.length >= 1) {
    const queryRegex = new RegExp(`^${req.query.q}.*`, 'i');
    suggestions = citiesData.filter(cityData => cityData.name.match(queryRegex));
  }

  if (req.query.latitude != null && req.query.longitude != null) {
    const userCoord = {
      latitude: req.query.latitude,
      longitude: req.query.longitude
    };
    suggestions.forEach(cityData => {
      const cityCoord = {
        latitude: cityData.latitude,
        longitude: cityData.longitude
      };
      cityData.distanceInKM = geolib.getDistance(userCoord, cityCoord) / 1000; //distance in kilometers
    })
  }

  suggestions.forEach(cityData => cityData.score = Math.pow(cityData.distanceInKM, -10/3) * Math.pow(cityData.population, 6));
  suggestions.sort((cityDataA, cityDataB) => cityDataB.score - cityDataA.score);
  const maxScore = suggestions.length >= 1 ? Math.log(suggestions[0].score) : null;
  const minScore = suggestions.length >= 1 ? Math.log(suggestions.slice(-1)[0].score) : null;
  suggestions.forEach(cityData => {
    cityData.score = Math.log(cityData.score);
    cityData.score -= minScore;
    cityData.score /= (maxScore-minScore);
    cityData.score = Math.round(cityData.score*100)/100;
  });

  if (suggestions.length <= 0) {
    res.status(404);
  }
  res.send({
    suggestions: suggestions
  });
});

app.use((req, res) => res.sendStatus(404)); //default 404 handler if no route found
app.listen(port, '127.0.0.1', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

module.exports = app;