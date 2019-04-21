const express = require('express');
const app = express();
const dataUtils = require('./data-utils');
const scoringHelper = require('./scoring-helper');
const port = process.env.PORT || 2345;

let citiesData = require('./sync-load-data');
citiesData = dataUtils.makeRegionsReadable(citiesData);
citiesData = dataUtils.renameLatLong(citiesData);
citiesData = dataUtils.addEasyDisplayName(citiesData);

app.get('/suggestions', (req, res) => {
  let suggestions = [];

  if (req.query.q != null && req.query.q.length >= 1) {
    const compareOptions = {usage: 'search', sensitivity: 'base'};
    suggestions = citiesData.filter(cityData => 0 === cityData.name.localeCompare(req.query.q, [], compareOptions));
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