const express = require('express');
const slowDown = require('express-slow-down');
const dataUtils = require('./data-utils');
const scoringHelper = require('./scoring-helper');

const app = express();
const port = process.env.PORT || 2345;

app.enable('trust proxy'); // trust `X-Forwarded-` HTTP headers, since we're behind a reverse proxy on Heroku

// create rate-limiting middleware, which will delay requests that are coming in too fast
const speedLimiter = slowDown({
  windowMs: 60 * 1000, // 1 minute
  delayAfter: 100, // allow 100 requests per minute, then...
  delayMs: 100 // begin adding 100ms of delay per request above 100:
});

let citiesData = require('./sync-load-data'); // load the data set
dataUtils.dropUnusedDataFields(citiesData) // drop unused fields to reduce its size
  .then(dataUtils.filterByCountry) // keep only cities from USA & Canada
  .then(dataUtils.filterByPopulation) // keep only cities with population >= 5000
  .then(dataUtils.sortDataByPopulationDesc)// sort by population, descending
  .then(dataUtils.replaceRegionCodesWithNames) // replace province/state and country codes with actual names
  .then(dataUtils.renameLatLong) // rename lat, long -> latitude, longitude
  .then(dataUtils.addEasyDisplayName) // add a geographically-qualified name like 'Montreal, Quebec, Canada'
  .then(processedCitiesData => { citiesData = processedCitiesData; }) // save the final processed data set
;

app.use(speedLimiter); // rate-limit requests, per challenge requirements
app.get('/suggestions', (req, res) => {
  let suggestions = [];

  if (req.query.q != null && req.query.q.length >= 1) {
    // create a case-insensitive regex of our query, as this is probably the easiest way to check strings
    const nameQueryRegex = new RegExp(`^${req.query.q}.*`, 'i');

    suggestions = citiesData.filter(cityData => // filter the data set to find matching entries
      cityData.name.match(nameQueryRegex) || // with the query in the name,
        cityData.ascii.match(nameQueryRegex) || // with the query in the ascii name (ignores accents),
        cityData.alt_name.some(altName => altName.match(nameQueryRegex)) // or with the query as one of the alternative names
    );
  }

  // clone each suggestion so that we can modify (add properties) them without affecting our original/raw data
  suggestions = suggestions.map(cityData => Object.assign({}, cityData));

  scoringHelper.addDistanceToSuggestions(suggestions, req.query.latitude, req.query.longitude);
  scoringHelper.scoreSuggestions(suggestions); // apply scores, normalise and sort

  if (suggestions.length <= 0) {
    res.status(404);
  }
  res.send({
    suggestions: suggestions
  });
});

app.use((req, res) => res.sendStatus(404)); // default 404 handler if no route found
app.listen(port, '127.0.0.1', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

module.exports = app;
