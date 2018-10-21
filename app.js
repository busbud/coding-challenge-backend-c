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
    list = sortByDistanceAndName(list);
  })
  //add scoring based on sorted distance
  for( let x=0; x<list.length; x++){
    list[x].score -= .1 * x ;
  }
  //reformatting city objects to imitate challenge example
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

      res.json(suggestions);
      return res.end();
    });
});

// const america = {
//   Alabama: AL,
//   Alaska: AK,
//   Arizona: AZ,
//   Arkansas: AR,
//   California: CA,
//   Colorado: CO,
//   Connecticut: CT,
//   Delaware: DE,
//   District of Columbia: DC,
//   Florida: FL,
//   Georgia: GA,
//   Hawaii: HI,
//   Idaho: ID,
//   Illinois: IL,
//   Indiana: IN,
//   Iowa: IA,
//   Kansas: KS,
//   Kentucky: KY,
//   Louisiana: LA,
//   Maine: ME,
//   Maryland: MD,
//   Massachusetts: MA,
//   Michigan: MI,
//   Minnesota: MN,
//   Mississippi: MS,
//   Missouri: MO,
//   Montana: MT,
//   Nebraska:, NE
//   Nevada: NV,
//   New Hampshire: NH,
//   New Jersey: NJ,
//   New Mexico: NM,
//   New York: NY,
//   North Carolina: NC,
//   North Dakota: ND,
//   Ohio: OH,
//   Oklahoma: OK,
//   Oregon: OR,
//   Pennsylvania: PA,
//   Rhode Island: RI,
//   South Carolina: SC,
//   South Dakota: SD,
//   Tennessee: TN,
//   Texas: TX,
//   Utah: UT,
//   Vermont: VT,
//   Virginia: VA,
//   Washington: WA,
//   West Virginia: WV,
//   Wisconsin: WI,
//   Wyoming: WY
// }

// const canada = {
//   Alberta: AB,
//   British Columbia:  BC,
//   Manitoba:  MB,
//   New Brunswick: NB,
//   Newfoundland and Labrador: NL,
//   Northwest Territories: NT,
//   Nova Scotia: NS,
//   Nunavut: NU,
//   Ontario: ON,
//   Prince Edward Island:  PE,
//   Quebec:  QC,
//   Saskatchewan:  SK,
//   Yukon: YT
// }

app.listen(port, (err) => {
  if (err) {
    return console.log('err', err)
  }

  console.log(`Server running at http://127.0.0.1:${port}/suggestions`);
})