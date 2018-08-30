// Core server dependencies
const express = require('express');
const bodyParser = require('body-parser');
const http = require('http');

// fetch and calculate distances and scores
const {getUserLL, getScore} = require('./helpers');

// data and data model
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
    // i for ignore case. Match cities that start with the search string.
    const searchRegex = new RegExp("^" + searchString, "i");

    // Try to get the User's Lat Lng from their IP-address
    getUserLL().then(LL => {
      let userLat = null;
      let userLng = null;
      if (LL) {
        userLat = LL.userLat;
        userLng = LL.userLng;
      }

      // exclude the _id and _v fields added by mongodb from the return
      Suggestion.find({ name: searchRegex }, '-_id -__v').then(cities => {
        const suggestions = cities.map(city => {
          const newName = city.name + ', ' + city.country
          const score = getScore(city, userLat, userLng);
          return {
            name: newName,
            latitude: city.latitude,
            longitude: city.longitude,
            score
          }
        }).sort((a, b) => {
          return b.score - a.score;
        });

        res.send({suggestions});
      }, (err) => {
        res.status(400).send(err);
      });
    });
  } else {
    res.end(JSON.stringify({
      suggestions: []
    }));
  }
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
})

module.exports = http.createServer(app).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
