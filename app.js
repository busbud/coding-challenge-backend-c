var http = require('http');
const url = require('url');
const querystring = require('querystring');
var rawData = require('./data/cities_canada-usa.json')
var constants = require('./src/constants.js')
var strinScore = require('./src/getStringScore')
var distance = require('./src/calculateDistance')
var port = process.env.PORT || 2345;

//Methods definition
function calculateOveralScore(distanceScore, stringScore){
  return (distanceScore + stringScore) / 2;
}

module.exports = http.createServer(function (req, res) {
  var parsedUrl = url.parse(req.url);
  var parsedQs = querystring.parse(parsedUrl.query);

  var query = {
    q:parsedQs.q,
    latitude:parsedQs.latitude,
    longitude:parsedQs.longitude
  };

  if (req.url.indexOf('/suggestions') === 0) {

    if(query.q && query.q.length > 0) {
      let matches = rawData.filter((city) => {
        return city.name.indexOf(parsedQs.q) > -1 || city.alt_name.indexOf(parsedQs.q) > -1;
      })

      if(
        isFinite(query['latitude']) && Math.abs(query['latitude']) <= 90 &&
        isFinite(query['longitude']) && Math.abs(query['longitude']) <= 180
      ) {
        matches.forEach(function(city){
          city['distance'] = distance(city["lat"],city["long"],parsedQs.latitude, parsedQs.longitude);
        })
      } else {
        matches.forEach(function(city){
          city['distance'] = Infinity;
        })
      }
      
      matches.sort((cityA, cityB) => cityB.distance - cityA.distance);
      var maxDist = matches.length > 0 ? matches[0]['distance']: 0;

      var results = [];
      matches.forEach(function(city){
        var sc = Math.round(calculateOveralScore(1 - (city['distance']/maxDist),strinScore.score(city['name'], parsedQs.q)) * 10)/10;
        city['score'] = maxDist !== Infinity? sc:Math.round(strinScore.score(city['name'], parsedQs.q) * 100)/100;
        
        let region = city['country'] == 'CA' ? constants.CANADA_PROVINCES[city['admin1']] : city['admin1'];
        let country = city['country'] == 'CA' ? constants.COUNTRY_NAME.CA :constants.COUNTRY_NAME.US;

        results.push(
          {
            "name": city["name"] + ', ' + region + ', ' + country,
            "latitude": city["lat"],
            "longitude": city["long"],
            "score": city['score']
          }
        )
      })

      results.sort((cityA, cityB) => cityB.score - cityA.score)
      if (results.length > 0) {
        res.writeHead(200, { "Content-Type": "text/plain" });
      } else {
        res.writeHead(404, { "Content-Type": "text/plain" });
      } 
      
      res.end(
        JSON.stringify({
          suggestions: results,
        })
      ); 

    } else{
      res.writeHead(400, {'Content-Type': 'text/plain'});
      res.end();
    }
    
  } else {
    res.writeHead(404, {'Content-Type': 'text/plain'});
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);