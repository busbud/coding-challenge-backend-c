const http = require('http');
const port = process.env.PORT || 2345;

const express = require('express');

const app = express();
const fs = require('fs');
const bodyParser = require('body-parser');
const url = require('url');
const querystring = require('querystring');
// var suggestionsRouter = require('./api/suggestions');

function calculateDistance(lat1, lon1, lat2, lon2) {
  var p = 0.017453292519943295;    // Math.PI / 180
  var c = Math.cos;
  var a = 0.5 - c((lat2 - lat1) * p)/2 +
          c(lat1 * p) * c(lat2 * p) *
          (1 - c((lon2 - lon1) * p))/2;

  return (12742 * Math.asin(Math.sqrt(a))).toFixed(2); // 2 * R; R = 6371 km
}
function isNorthAmerica(item) {
  if (item.country == 'US' ||  item.country == 'CA'){
    return true;
  }
}
function filterResults(item) {
  if (item.population > 5000 && isNorthAmerica(item)) {
    return true;
  }
}
function findCity(data, search) {
  let list = [];
   data.forEach(function(city) {
    let cityName = (city.name).toLowerCase();
    let distance = calculateDistance(city.latitude, city.longitude, search.latitude, search.longitude)
    if(cityName.startsWith(search.name)){
        list.push({
            name: city.name,
            area: city.area,
            country: city.country,
            latitude: city.latitude,
            longitude: city.longitude,
            distance: distance})
    }
  })
   return list;
}
function sortByScore(results) {
   function compare(a,b) {
      if (a.distance < b.distance)
        return -1;
      if (a.distance> b.distance)
        return 1;
      return 0;
    }
  results.sort(compare);
return results;
}


app.get('/suggestions', function(req, res) {
    if (req.url.indexOf('/suggestions') === 0) {
    res.status(404);
      }
    res.setHeader('Content-Type', 'application/json')

     fs.readFile('./data/cities_canada-us.json', function(err, data){
      let newArr =[];
      let filteredArray =[];
      let arr = JSON.parse(data);

      arr.forEach(function(city) {
        newArr.push(
          { name :city.name,
            area :city.tz,
            country: city.country,
            latitude: city.lat,
            longitude: city.long,
            population: city.population
          }
        )

        filteredArray = newArr.filter(filterResults)

      });
      let query = {
        name: req.query.q.toLowerCase(),
        latitude: req.query.latitude,
        longitude: req.query.longitude };

      let results = findCity(filteredArray, query)

      let suggestions = {
        suggestions: sortByScore(results)
      }

      res.json(suggestions);
      return res.end();
    });


});


app.listen(port, (err) => {
  if (err) {
    return console.log('err', err)
  }

  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
})