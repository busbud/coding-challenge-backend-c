const express = require('express');
const app = express();
const dataUtils = require('./data-utils');
const port = process.env.PORT || 2345;

let citiesData = require('./sync-load-data');
citiesData = dataUtils.makeRegionsReadable(citiesData);
citiesData = dataUtils.renameLatLong(citiesData);

app.get('/suggestions', (req, res) => {
  let potentialCityMatches = [];

  if (req.query.q != null && res.query.q.length >= 1) {
    const queryRegex = new RegExp(`^${req.query.q}.*`, 'i');
    potentialCityMatches = citiesData.filter(cityData => cityData.name.match(queryRegex));
  }

  if (potentialCityMatches.length <= 0) {
    res.status(404);
  }
  res.send({
    suggestions: potentialCityMatches
  });
});

app.use((req, res) => res.sendStatus(404)); //default 404 handler if no route found
app.listen(port, '127.0.0.1', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

process.on('unhandledRejection', error => {throw error});
process.on('uncaughtException', error => console.log(error));

module.exports = app; 