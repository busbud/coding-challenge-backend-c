const express = require('express');
const SlowDown = require('express-slow-down');
const date_utils = require('./data-utils');
const scoring_helper = require('./scoring-helper');

const app = express();
const port = process.env.PORT || 2345;

app.enable('trust proxy'); // trust `X-Forwarded-` HTTP headers, since we're behind a reverse proxy on Heroku

// create rate-limiting middleware, which will delay requests that are coming in too fast
const slow_down = SlowDown({
  windowMs: 60 * 1000, // 1 minute
  delayAfter: 100, // allow 100 requests per minute, then...
  delayMs: 100 // begin adding 100ms of delay per request above 100:
});

// load and process data
let cities_data = require('./sync-load-data');
date_utils.dropUnusedDataFields(cities_data)
  .then(date_utils.filterDataByCountry)
  .then(date_utils.filterDataByMinPopulation)
  .then(date_utils.sortDataByPopulationDesc)
  .then(date_utils.replaceRegionCodesWithNames)
  .then(date_utils.renameLatLong)
  .then(date_utils.addEasyDisplayName)
  .then(d => { cities_data = d; }) // save the final processed data set
;

app.use(slow_down); // rate-limit requests, per challenge requirements
app.get('/suggestions', (req, res) => {
  let suggestions = [];

  if (req.query.q != null && req.query.q.length >= 1) {
    // create a case-insensitive regex of our query, as this is probably the easiest way to check strings
    const name_query_regex = new RegExp(`^${req.query.q}.*`, 'i');

    suggestions = cities_data.filter(city_data => // filter the data set to find matching entries
      city_data.name.match(name_query_regex) || // with the query in the name,
        city_data.ascii.match(name_query_regex) || // with the query in the ascii name (ignores accents),
        city_data.alt_name.some(n => n.match(name_query_regex)) // or with the query as one of the alternative names
    );
  }

  // clone each suggestion so that we can modify (add properties) them without affecting our original/raw data
  suggestions = suggestions.map(c => Object.assign({}, c));

  scoring_helper.addDistanceToSuggestions(suggestions, req.query.latitude, req.query.longitude);
  scoring_helper.scoreSuggestions(suggestions); // apply scores, normalise and sort

  if (suggestions.length <= 0) { // if we can't find any suggestions
    res.status(404); // respond 404, per API requirements
  }
  res.send({
    suggestions: suggestions
  });
});

app.use((req, res) => res.sendStatus(404)); // default 404 handler if no route found
app.listen(port, '0.0.0.0', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

module.exports = app;
