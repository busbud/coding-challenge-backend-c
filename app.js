"use strict";
const http = require('http');
const url = require('url');
const fs = require('fs');
const path = require('path');
const fuzzysort = require('fuzzysort');
const geohash = require('latlon-geohash');

const port = process.env.PORT || 2345;

//function that tests if latitude and longitude is valid (takes input pre-converted to Number)
const testLatLon = function(latitude/*Number!*/,longitude/*Number!*/) {
  return (
  // watch out! false >= 0 === true
  (latitude === 0 || (latitude > 0 && latitude <= 90) || (latitude < 0 && latitude >= -90)) ||
  (longitude === 0 || (longitude > 0 && longitude <= 180) || (longitude < 0 && longitude >= -180))
  );
};

//function that builds two arrays of geohash neighbours (first with geohash, second with score)
const neighboursFunc = function(lat,long) {
  let mainGeohash = geohash.encode(lat, long, 3);
  let neighboursObj = geohash.neighbours(mainGeohash);
  let neighboursArr = Object.keys(neighboursObj).map(function(n) { return neighboursObj[n]; });
  let neighboursScoreArr = [];
  //deal with main geohash
  neighboursArr.unshift(mainGeohash);
  neighboursScoreArr[0] = 100;
  //deal with first neighbours
  for (let i=1;i<neighboursArr.length;i++){
    neighboursScoreArr[i] = 75;
  }
  //deal with second neighbours
  for (let i=1;i<9;i++){
    let secondNeighboursObj = geohash.neighbours(neighboursArr[i]);
    let secondNeighboursArr = Object.keys(secondNeighboursObj).map(function(n) { return secondNeighboursObj[n]; });
    for (let j=0;j<secondNeighboursArr.length;j++){
      if(neighboursArr.indexOf(secondNeighboursArr[j]) === -1){
        neighboursArr.push(secondNeighboursArr[j]);
        neighboursScoreArr.push(50);
      }
    }
  }
  return [neighboursArr,neighboursScoreArr];
};

//import provinces names at start-up
let provinces = fs.readFileSync(path.join(__dirname,'/data/admin1CodesASCII.tsv'),"utf8").split(/\r?\n/);
let provincesObject = {};
for (let i=0;i<provinces.length;i++){
  provinces[i] = provinces[i].split('\t');
  provincesObject[provinces[i].shift()] = provinces[i];
}
provinces = null;

//import data at start-up
let data = fs.readFileSync(path.join(__dirname,'/data/cities_canada-usa.tsv'),"utf8").split(/\r?\n/);
let dataArrayOfObjects = [];
data[0] = data[0].split('\t');
for (let i=1;i<data.length;i++){
  data[i] = data[i].split('\t');
  dataArrayOfObjects[i-1] = {};
  let country;
  let admin1;
  let name;
  let lat;
  let long;
  for (let j=1;j<data[i].length;j++){
    dataArrayOfObjects[i-1][data[0][j]] = data[i][j];
    if (data[0][j] === "country"){
      country = data[i][j];
    } else if (data[0][j] === "admin1"){
      admin1 = data[i][j];
    } else if (data[0][j] === "name"){
      name = data[i][j];
    } else if (data[0][j] === "lat"){
      lat = Number(data[i][j]);
    } else if (data[0][j] === "long"){
      long = Number(data[i][j]);
    }
  }
  if (name && country && admin1) {
    //prepare fullName
    dataArrayOfObjects[i - 1].fullName = name + ", " + provincesObject[country + "." + admin1][0] + ", " + (country === "US" ? "USA" : country === "CA" ? "Canada" : country);

    //prepare for fuzzySort (for performance)
    dataArrayOfObjects[i - 1].namePrepared = fuzzysort.prepare(name);
  }
  if (testLatLon(lat,long)){
    //add geoHash to object
    dataArrayOfObjects[i - 1].geoHash = geohash.encode(lat, long, 3);
  }
}
data = null;
provincesObject = null;

module.exports = http.createServer(function (req, res) {
  // Parse the url
  const parsedUrl = url.parse(req.url, true);

  // Get the path
  const path = parsedUrl.pathname;
  const trimmedPath = path.replace(/^\/+|\/+$/g, '');

  // Get the query string as an object
  const queryStringObject = parsedUrl.query;

  // Get the HTTP method
  const method = req.method.toUpperCase();

  if (  (trimmedPath === 'suggestions') &&
        (method === 'GET') ) {
    //query validation
    const q = typeof(queryStringObject.q) === 'string' && queryStringObject.q.trim().length && queryStringObject.q.trim().length <= 100 ? queryStringObject.q.trim() : false;
    const latitude = typeof(queryStringObject.latitude) == 'string' && queryStringObject.latitude.trim().length > 0 ? Number(queryStringObject.latitude.trim()) : false;
    const longitude = typeof(queryStringObject.longitude) == 'string' && queryStringObject.longitude.trim().length > 0 ? Number(queryStringObject.longitude.trim()) : false;

    let useLatLon = testLatLon(latitude,longitude);
    if (!q){
      res.writeHead(400, {'Content-Type': 'text/plain'});
      res.end('q missing');
    }  else if ((latitude || longitude) && !useLatLon){
      res.writeHead(422, {'Content-Type': 'text/plain'});
      res.end('invalid longitude/latitude');
    } else {
      let treshold = -10000;
      let options = {
        limit: 50,
        allowTypo: true,
        threshold: treshold,
        key: "namePrepared"/*,
        scoreFn: function(a) {console.log(JSON.stringify(a,null,2)); return a[0].score;}*/ //function seems to be broken -- scoreFn input always receives null
      };
      let results = fuzzysort.go(q,dataArrayOfObjects, options);

      //Work on score
      for (let i=0;i<results.length;i++){
        results[i].score = (treshold - results[i].score)/treshold;
      }
      //score if useLatLon is true (meaning that we have valid longitude and latitude in query)
      if (useLatLon) {
        //get neighbour arrays (first one with neighbours, second one with their score)
        let neighboursArrs = neighboursFunc(latitude, longitude);
        //loop over results
        for (let i = 0; i < results.length; i++) {
          if (results[i].obj && results[i].obj.geoHash) {
            let currentGeohash = results[i].obj.geoHash;
            let indexValue = neighboursArrs[0].indexOf(currentGeohash);
            results[i].score = (results[i].score) / 2;
            if (indexValue > -1) {
              results[i].score = results[i].score + (neighboursArrs[1][indexValue]) / 100 / 2;
            }
          }
        }
      }
      results = results.map(x => {
        return {
          "name": (x&&x.obj&&x.obj.fullName ? x.obj.fullName : ""),
          "latitude": (x&&x.obj&&x.obj.lat ? x.obj.lat : ""), //lat is string, so 0 is truthy
          "longitude": (x&&x.obj&&x.obj.long ? x.obj.long : ""), //long is string, so 0 is truthy
          "score": (x&&x.score ? x.score : 0)
        };
      });
      results.sort(function(a, b){return b.score - a.score});
      res.writeHead(200, {'Content-Type': 'application/json'});
      res.end(JSON.stringify({
        suggestions: results
      }));
    }
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');
console.log('Server running at http://127.0.0.1:%d/suggestions', port);