const express = require('express');
const fs = require('fs');
const fcs = require('./search/FuzzyCitySearch');

const provinces = {
  '01': 'AB',
  '02': 'BC',
  '03': 'MB',
  '04': 'NB',
  '05': 'NL',
  '07': 'NS',
  '08': 'ON',
  '09': 'PE',
  '10': 'QC',
  '11': 'SK',
  '12': 'YK',
  '13': 'NT',
  '14': 'NU',
};

let cities = [];

const lineReader = require('readline').createInterface({
  input: require('fs').createReadStream('./data/cities_canada-usa.tsv')
});

lineReader.on('line', function (line) {
  const elements = line.split("\t");

  // skip the header
  if (elements[0] === 'id') {
    return;
  }

  // skip cities with less than 5000 population
  if (elements[14] < 5000) {
    return;
  }

  // skip cities outside of the USA and Canada
  if (elements[8] !== 'US' && elements[8] !== 'CA') {
    return;
  }

  const country = ((countryCode) => {
    switch(countryCode) {
      case 'US': return 'USA';
      case 'CA': return 'Canada';
    }
  })(elements[8]);

  const state = ((countryCode, stateCode) => {
    switch(countryCode) {
      case 'US': return stateCode;
      case 'CA': return provinces[stateCode];
    }
  })(elements[8], elements[10]);

  const city = {
    name: `${elements[2]}, ${state}, ${country}`,
    latitude: `${elements[4]}`,
    longitude:`${elements[5]}`,
  }

  cities.push(city);
});

const fuzzyCitySearch = new fcs(cities, ['name'], { caseSensitive:false, sort:true });

const app = express()
const port = process.env.PORT || 2345;
app.get('/suggestions', (request, response) => {

  const result = fuzzyCitySearch.search(request.query.q, request.query.latitude, request.query.longitude);

  if (result.length === 0) {
    response.status(404);
  }

  response.setHeader('Content-Type', 'application/json')
  response.send(JSON.stringify({ suggestions: result }));
})

app.listen(port, (err) => {
  if (err) {
    return console.log('something bad happened', err)
  }

  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
})

module.exports = app;
