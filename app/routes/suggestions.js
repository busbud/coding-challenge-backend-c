import DataStore from '../datastore';
import express from 'express';
import client from '../elastic.js';
import stringSimilarity from 'string-similarity';

const store = new DataStore(client);

const app = express();
const provinceMapping = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '13': 'NT',
  '14': 'NU',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YT',
};

/**
* Simple helper to format a city
*/
function formatCityName(city) {
  return city.name + ', ' + (city.country ==='CA' ? provinceMapping[city.admin_1] : city.admin_1) + ', ' + city.country;
}

/**
* Simple helper to check a result against original query (could be improved with population and geo)
*/
function confidence(result, query, geo = null) {
  return stringSimilarity.compareTwoStrings(result._source.name, query);
}

app.get('/suggestions', (req, res) => {

  var geo = null;

  if (req.query.lat && req.query.lng) {
    geo = {lat: req.query.lat, lng: req.query.lng};
  }

  store.search(req.query.q, geo)
    .then((results) => {
      if (results.length === 0) {
        return res.status(404).send({suggestions: []});
      }

      res.send({ suggestions: results.map((result) => {
        const city = result._source;

        return {
          'name': formatCityName(city),
          'population' : city.population,
          'score': confidence(result, req.query.q, geo),
          '_score': result._score,
          'latitude': city.location.lat,
          'longitude': city.location.lon
        };
      })});
    }, (err) => {
      console.log('ERROR', err);
      res.status(500).send({error: err.message});
    });

});

export default app;
