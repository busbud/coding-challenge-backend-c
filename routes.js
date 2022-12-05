const express = require('express');
const DataParser = require('./utils/DataParser');
const router = express.Router();
const { getScore } = require('./utils/ScoreCalculator');

const dataParser = new DataParser('cities_canada-usa.tsv', {
  desiredCountries: ['CA', 'US'],
  desiredPopulation: 5000
});
const data = dataParser?.data;

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

    if (!locationQuery) {
      return res.status(404).send({ suggestions: [] });
    }

    const result = [];
    for (let i = 0; i < data.length; i++) {
      const obj = data[i];
      const score = getScore(obj, locationQuery, { latitude, longitude });
      if (score >= 0.3) {
        result.push(serializeObject(obj, score));
      }
    }
    result.sort((a, b) => b?.score - a?.score);


    if (!result?.length) {
      return res.status(404).send({ suggestions: [] });
    }

    if (result?.length) {
      return res.status(200).send({ suggestions: result });
    }
    
    return res.status(500).send('Error');
  });

  router.get('/*', (req, res) => {
    res.status(404).send('Route not found');
  });

  return router;
}

module.exports = routes;
