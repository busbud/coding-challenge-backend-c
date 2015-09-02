import DataStore from '../store/datastore';
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

  if (req.query.latitude && req.query.longitude) {
    geo = {lat: req.query.latitude, lng: req.query.longitude};
  }

  if (!req.query.q) {
    return res.status(400).send('Bad request');
  }

  store.search(req.query.q, geo)
    .then((results) => {

      const responseData = {
        suggestions: results.map((result) => {
          const city = result._source;

          return {
            'name': formatCityName(city),
            'population' : city.population,
            'score': confidence(result, req.query.q, geo),
            '_score': result._score,
            'latitude': city.location.lat,
            'longitude': city.location.lon
          };
        })
      };
      res.set('Cache-Control', 'public, max-age=86400');

      if (results.length === 0) {
        return res.status(404).json(responseData);
      }

      res.json(responseData);
    }, (err) => {
      console.log('ERROR', err);
      res.status(500).json({error: err.message});
    });

});

export default app;
