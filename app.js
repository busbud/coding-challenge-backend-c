const express = require('express');
const app = express();

const port = process.env.PORT || 2345;
const data = require('./data');
const scoreCalculator = require('./scoreCalculator');

const Triejs = require('triejs');
const trie = new Triejs();

let cityData;

app.listen(port, () => {
  console.log('App listening on port : ' + port);
  cityData = data.loadData(trie); // load cities..
});

app.get('/', (req, res) => {
  res.send('BusBud AutoSugest API!');
})

//GET /suggestions?q=Londo&latitude=43.70011&longitude=-79.4163
app.get('/suggestions', (req, res) => {
  const queryParam = req.query.q;
  const longitude = req.query.longitude;
  const latitude = req.query.latitude;

  let result = [];
  if (queryParam) {
    result = trie.find(queryParam.toLowerCase());
  }

  if (result) {
    result = scoreCalculator.calculateScore(result, queryParam, latitude, longitude);
  }

  res.status(result ? 200 : 404).send(JSON.stringify({
    suggestions: result || []
  }))
})

module.exports = app;