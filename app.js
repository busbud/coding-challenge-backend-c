const express = require('express');
const bodyParser = require('body-parser');
const http = require('http');

const jsonCities = require('./data/cities.json');

const {Suggestion} = require('./data/suggestion');

const app = express();
const port = process.env.PORT || 2345;

// Tells express to expect JSON
app.use(bodyParser.json());

app.get('/suggestions', (req, res) => {
  const searchString = req.query.q;
  const latitude = req.query.latitude;
  const longitude = req.query.longitude;

  if (searchString) {
    
  } else {
    res.end(JSON.stringify({
      suggestions: []
    }));
  }

  res.end();
})

// Load the JSON data into a MongoDB database
app.post('/suggestions', (req, res) => {
  const cities = jsonCities.cities
    .map((city) => { // return only the relevant information we want
      return({
        name: city.name,
        latitude: city.lat,
        longitude: city.long,
        population: city.population,
        country: city.country
      })
    });

  // A more scale-able solution to posting large sets of data
  const batchSize = 100;
  const batchCount = Math.ceil(cities.length / batchSize);
  let ops = [];
  let counter = 0;
  for (let i = 0; i < batchCount; i++) {
    let batch = cities.slice(counter, counter + batchSize);
    counter += batchSize;
    ops.push(Suggestion.insertMany(batch));
  }

  Promise.all(ops).then((doc) => {
    res.send(doc);
  }, (err) => {
    res.status(400).send(err);
  });

  // Suggestion.insertMany(cities).then((error, doc) => {
  //   res.send(doc);
  // }, (err) => {
  //   res.status(400).send(err);
  // });
})

module.exports = http.createServer(app).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
