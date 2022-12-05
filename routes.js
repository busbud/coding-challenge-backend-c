const express = require('express');
const DataParser = require('./utils/DataParser');
const router = express.Router();
const { getScore } = require('./utils/ScoreCalculator');
const { COUNTRY_CODES } = require('./constants');

// Parse the data from TSV to JSON
const dataParser = new DataParser('cities_canada-usa.tsv', {
  desiredCountries: [COUNTRY_CODES.CA, COUNTRY_CODES.US],
  desiredPopulation: 5000
});
const data = dataParser?.data; // store data in memory

// this serialize function will serialize each object into the
// correct format before sending it as a result
const serializeObject = (obj = {}, score = 0) => {
  return ({
    name: obj?.name,
    latitude: obj?.latitude,
    longitude: obj?.longitude,
    score: +score?.toFixed(2)
  });
}

const routes = function () {

  router.get('/', (req, res) => {
    const { q, latitude, longitude } = req?.query;
    const locationQuery = q?.toLowerCase();

    // if no location was queried return 404 and an empty array
    if (!locationQuery) {
      return res.status(404).send({ suggestions: [] });
    }

    // here let's loop through the data and calculate scores
    const result = [];
    for (let i = 0; i < data.length; i++) {
      const obj = data[i];
      const score = getScore(obj, locationQuery, { latitude, longitude }); // we can pass latitude and longitude as options to boost score
      // if the score is greater than 0.3 let's serialize the object and push to the results array
      if (score >= 0.3) {
        result.push(serializeObject(obj, score));
      }
    }
    result.sort((a, b) => b?.score - a?.score);

    // if no results found return 404 and an empty array
    if (!result?.length) {
      return res.status(404).send({ suggestions: [] });
    }

    // otherwise return 200 and the results array
    if (result?.length) {
      return res.status(200).send({ suggestions: result });
    }

    // if no results we must have had an error
    return res.status(500).send({ message: 'Error!' });
  });

  // handle unkown routes
  router.get('/*', (req, res) => {
    res.status(404).send({ message: 'Route not found' });
  });

  return router;
}

module.exports = routes;
