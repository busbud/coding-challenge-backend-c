const express = require('express');
const slowDown = require("express-slow-down");
const app = express();
const dataUtils = require('./data-utils');
const scoringHelper = require('./scoring-helper');
const port = process.env.PORT || 2345;
app.enable("trust proxy");

const speedLimiter = slowDown({
  windowMs: 60 * 1000, // 1 minute
  delayAfter: 100, // allow 100 requests per minute, then...
  delayMs: 100 // begin adding 100ms of delay per request above 100:
});

let citiesData = require('./sync-load-data');
dataUtils.dropUnusedDataFields(citiesData)
  .then(dataUtils.filterByCountry)
  .then(dataUtils.filterByPopulation)
  .then(dataUtils.sortDataByPopulation)
  .then(dataUtils.makeRegionsReadable)
  .then(dataUtils.renameLatLong)
  .then(dataUtils.addEasyDisplayName)
  .then(processedCitiesData => citiesData = processedCitiesData)
;

app.use(speedLimiter); //rate-limit requests, per challenge requirements
app.get('/suggestions', (req, res) => {
  let suggestions = [];

  if (req.query.q != null && req.query.q.length >= 1) {
    const nameQueryRegex = new RegExp(`^${req.query.q}.*`, 'i');
    suggestions = citiesData.filter(cityData =>
      cityData.name.match(nameQueryRegex)
        || cityData.ascii.match(nameQueryRegex)
        || cityData.alt_name.some(altName => altName.match(nameQueryRegex))
    );
  }

  //clone each suggestion so that we can modify them without affecting our original/raw data
  suggestions = suggestions.map(cityData => Object.assign({}, cityData));

  scoringHelper.addDistanceToSuggestions(suggestions, req.query.latitude, req.query.longitude);
  scoringHelper.scoreSuggestions(suggestions);

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