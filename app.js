const http = require('http');
const port = process.env.PORT || 2345;

const express = require('express');

const app = express();
const fs = require('fs');
const bodyParser = require('body-parser');
const url = require('url');
const querystring = require('querystring');


//BASIC FILTER -- filter results by US & Canada only and by minimum 5000 population
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

//Haversine formula
function calculateDistance(lat1, lon1, lat2, lon2) {
  var p = 0.017453292519943295;    // Math.PI / 180
  var c = Math.cos;
  var a = 0.5 - c((lat2 - lat1) * p)/2 +
          c(lat1 * p) * c(lat2 * p) *
          (1 - c((lon2 - lon1) * p))/2;

  return (12742 * Math.asin(Math.sqrt(a))).toFixed(2); // 2 * R; R = 6371 km
}
function reformat(list){
    //reformatting city objects to imitate challenge example
  if(list.length){
  list.forEach(function(city){
      let temp = city.name;
      let temp2 = city.country;
      let nameCountry = temp + ' , ' + temp2;
      city.name = nameCountry;
      delete city.distance;
      delete city.country;
    })
    return list;
  }
  return list;
}
function findCity(data, search) {
  let list = [];

  data.forEach(function(city) {
    let cityName = (city.name).toLowerCase();
    let distance = calculateDistance(city.latitude, city.longitude, search.latitude, search.longitude)
    if(cityName.startsWith(search.name)){
      list.push({
        name: city.name,
        country: city.country,
        latitude: city.latitude,
        longitude: city.longitude,
        distance: distance,
        score: 1})
    }
  })
  if (list.length > 0){
    list = sortByDistanceAndName(list);
      list = list.slice(0,10); //get only top 10 results
      //add scoring based on sorted distance
      for( let x=0; x<list.length; x++){
        list[x].score -= ((1/list.length) * x).toFixed(2);
      }
      let firstMatch = list[0].name.toLowerCase();

      if(firstMatch == search.name){
      list = [list[0]];

      return reformat(list);
    }else{
      return reformat(list);
    }
  }else{
    return list;
  }
}
function sortByDistanceAndName(results) {
  results.sort(function (x, y){
    var n = x.name.length - y.name.length;
      if( n!== 0 ){
        return n;
      }
    return x.distance - y.distance;
  });
return results;
}


app.get('/suggestions', function(req, res) {
if (req.url.indexOf('/suggestions') === 0) {
    fs.readFile('./data/cities_canada-us.json', function(err, data){
      let newArr =[];
      let filteredArray =[];
      let arr = JSON.parse(data);

      arr.forEach(function(city) {
        newArr.push(
          { name :city.ascii,
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
        longitude: req.query.longitude
      };

      let results = findCity(filteredArray, query)

      let suggestions = {
        suggestions: results
      }
    if (results.length == 0) {
      res.status(404);
    }
     res.setHeader('Content-Type', 'application/json')
     res.json(suggestions);
    });
  }else {
    res.status(404);
    res.setHeader('Content-Type', 'application/json')
    res.end();
  }
});

app.listen(port, (err) => {
  if (err) {
    return console.log('err', err)
  }

  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
})

module.exports = app;