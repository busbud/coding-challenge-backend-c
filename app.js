// Core server dependencies
const express    = require('express');
const bodyParser = require('body-parser');
const http       = require('http');

// fetch and calculate distances and scores
const {getUserLL, getScore} = require('./helpers');

// data and data model
const jsonCities   = require('./data/cities.json');
const {Suggestion} = require('./data/suggestion');

const app  = express();
const port = process.env.PORT || 2345;

// Tells express to expect JSON
app.use(bodyParser.json());

// Fetch matching cities sorted by descending score
app.get('/suggestions', (req, res) => {
  const searchString = req.query.q;
  const latitude = req.query.latitude;
  const longitude = req.query.longitude;

  if (searchString) {
    // i for ignore case. Match cities that start with the search string.
    // We also process the search string to match characters with diacritics
    // I don't know what all the accents used in canadian city names are, Sorry!
    const searchRegex = new RegExp("^" + searchString.replace('e','(e|é)'), "i");

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
          const newName = city.name + ', ' + city.country;

          let score = 0;
          if (latitude && longitude) {
            // If the user supplied their own lat lng, use that
            score = getScore(city, latitude, longitude);
          } else {
            score = getScore(city, userLat, userLng);
          }

          return {
            name: newName,
            latitude: city.latitude,
            longitude: city.longitude,
            score
          }
        }).sort((a, b) => {
          return b.score - a.score;
        });

        // Check if there were any cities that matched the search string
        if (suggestions.length !== 0) {
          res.status(200).send({suggestions});
        } else {
          res.status(404).send({suggestions});
        }
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
