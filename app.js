var http = require('http');
var port = process.env.PORT || 2345;
const url = require('url');
const fs = require('fs').promises;

//pull info from .tsv to an array of objects to work with
async function getCities() {
  const data = await fs.readFile('./data/cities_canada-usa.tsv', 'utf-8');  
  return cities = data.split('\n').map(city => ({
      id: city.split('\t')[0],
      name: city.split('\t')[2],
      latitude: city.split('\t')[4],
      longitude: city.split('\t')[5],
      countryCode: city.split('\t')[8],
      admin1: city.split('\t')[10],
      population: city.split('\t')[14]
    }));
};

//filter by population, country code and text from the query
async function filter(query) {
  const cities = await getCities();
  query.q = query.q.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  const filteredCities = cities.filter(
    city =>
    city.population > 5000 &&
    (city.countryCode == 'US' || 'CA') &&
      city.name.toLowerCase().includes(query.q)
  );
  //once filtered, score the cities
  for (i = 0; i < filteredCities.length; i++) {
    score(filteredCities[i], query);
  }
  //once filtered and scored, make the data pretty
  for (i = 0; i < filteredCities.length; i++) {
   filteredCities[i] = pretty(filteredCities[i]);
  }
  const sortedSuggestions = filteredCities.sort((a, b) => b.score - a.score);
  return sortedSuggestions;
}

//Don't know how to merge two tsv files (to get admin1 codes from http://download.geonames.org/export/dump/admin1CodesASCII.txt) so using brute force
function pretty(city) {
  const provinces = {
    '01': 'Alberta',
    '02': 'British Columbia',
    '03': 'Manitoba',
    '04': 'New Brunswick',
    '05': 'Newfoundland and Labrador',
    '07': 'Nova Scotia',
    '08': 'Ontario',
    '09': 'Prince Edward Island',
    '10': 'Quebec',
    '11': 'Saskatchewan',
    '12': 'Yukon',
    '13': 'Nunavut',
    '14': 'British Columbia',
  }
  for (let key in provinces) {
    if (city.admin1 == key) {
      city.admin1 = provinces[key];
    }
  }
  const newCity = {
    name: `${city.name}, ${city.admin1}, ${city.countryCode}`,
    latitude: city.latitude,
    longitude: city.longitude,
    score: Math.floor(city.score *10)/10,
  }
  return newCity;
}


//algorithm for scroing the cities
function score(city, query) {
  city.score = 0.1;

  //if exact match
  if (query.q == city.name.toLowerCase()) {
    city.score += 0.5;
  }
  //more points for a larger city
  if (city.population > 100000) {
    city.score += 0.1;
  }
  //if city and query start with the same letter
  if (query.q[0] == city.name[0].toLowerCase()) {
    city.score += 0.1;
  }
  //up the points if there is a lat or long that is close
  if (query.latitude) {
    if (parseFloat(query.latitude) - parseFloat(city.latitude) < 1) {
      city.score += 0.1;
    }
  }
  if (query.longitude) {
    if (parseFloat(query.longitude) - parseFloat(city.longitude) < 1) {
      city.score += 0.1;
    }
  }
}


module.exports = http.createServer(function(req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
      async function query() {
        const parsedUrl = url.parse(req.url, true);
        const query = parsedUrl.query;

          //if no query provided
          if (!query) {
            res.end('No valid search entered');
          } else if (!query.q){
            res.end('No valid search entered');
          }

        return await filter(query);
      }
      query().then(suggestions => { 
        if (!suggestions) {
          //setting the statusCode does not seem to be working, sorry
          res.writeHead(404, {'Content-Type': 'text/html'});
          res.end(
            JSON.stringify({
              suggestions: [],
            })
          );
        } else {
          res.statusCode = 200;
          res.end(
            JSON.stringify({
              suggestions: suggestions,
            })
          );
        }});
    } else {
      res.statusCode = 404;
      res.end();
    }
  })
  .listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
