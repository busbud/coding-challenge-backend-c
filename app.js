const express = require('express');
const app = express();

const port = process.env.PORT || 2345;
const data = require('./data');
const scoreCalculator = require('./scoreCalculator');

var TrieSearch = require('trie-search');
const trie = new TrieSearch();

var RateLimit = require('express-rate-limit');

app.enable('trust proxy');

var limiter = new RateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  delayMs: 0 // disable delaying - full speed until the max limit is reached
});

//  apply to all requests
app.use(limiter);


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

  let result;
  if (queryParam) {
    result = trie.get(queryParam.toLowerCase());
  }

  if (result) {
    result = scoreCalculator.calculateScore(result, queryParam, latitude, longitude);
  }

  res.status(result && result.length > 0 ? 200 : 404).send(JSON.stringify({
    suggestions: result || []
  }))
})

module.exports = app;