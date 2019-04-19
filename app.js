const express = require('express');
const app = express();
const dataUtils = require('./data-utils');
const port = process.env.PORT || 2345;

let citiesData = require('./sync-load-data');
citiesData = dataUtils.makeRegionsReadable(citiesData);

app.get('/suggestions', (req, res) => {
  const suggestions = [];
  res.send({
    suggestions: suggestions
  });
});
app.use((req, res) => res.sendStatus(404));
app.listen(port, '127.0.0.1', () => console.log('Server running at http://127.0.0.1:%d/suggestions', port));

process.on('unhandledRejection', error => {throw error});
process.on('uncaughtException', error => console.log(error));

module.exports = app; 