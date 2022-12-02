const express = require('express');
const DataParser = require('../utils/DataParser');
const router = express.Router();

const dataParser = new DataParser('cities_canada-usa.tsv', {
  desiredCountries: ['CA', 'US'],
  desiredPopulation: 5000
});
const data = dataParser?.data;

const routes = function () {

  router.get('/', (req, res) => {
    const { q: location } = req?.query;

    let result = [];
    for (let i = 0; i < data.length; i++) {
      const obj = data[i];
      if (obj?.name?.toLowerCase() === location?.toLowerCase()) {
        result.push(obj);
      }
    }
    console.log(result);

    if (!result?.length) {
      return res.status(404).send([]);
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
