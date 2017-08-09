var http = require('http');
var url = require('url');
var _ = require('lodash');
var cities = require('./cities/usa-ca.json');

var port = process.env.PORT || 2345;

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});
  var url_parts = url.parse(req.url, true);
  var query = url_parts.query;

  if (req.url.indexOf('/suggestions') === 0) {
    
    if(query && query.q){
      var requestedCity = query.q;
      RegExp.prototype.toJSON = function() { return this.source; };

      var citiesFound = _.filter(cities, function(e) { 
        return cleanPartialCityName(e.name).indexOf(cleanPartialCityName(requestedCity)) !==-1 ; 
      });

      if(citiesFound.length > 0 ){
        var returnCities = [];
        _.forEach(citiesFound, function(myCity){
          var score;
          if(query.longitude && query.latitude){
            var distance = calcDistance(query.latitude, query.longitude, myCity.latitude, myCity.longitude);
            score = calcScoreFromDistance(distance);
          }else{
            score = calcScoreFromVariableLength(requestedCity, myCity.name);
          }
          returnCities.push(
            {
            name: removeAccent(myCity.name)+', '+myCity.stateCode+', '+myCity.countryCode,
            latitude: myCity.latitude,
            longitude: myCity.longitude,
            score: score
          }
        );
        });

        returnCities.sort(sortByScore);

        res.writeHead(200, {'Content-Type': 'application/json; charset=utf-8'});
        res.end(JSON.stringify({
          suggestions: returnCities
        }));
      }else{
        res.end(JSON.stringify({
          suggestions: []
        }));
      }
    
    }else{
      res.end(JSON.stringify({
        suggestions: []
      }));
    }
     
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');
 
var cleanPartialCityName = function(myString){
  myString = removeAccent(myString);
  return myString.toLowerCase();
}
var removeAccent = function(myString){
  myString = myString.replace(/[àáââ]+/g, 'a');
  myString = myString.replace(/[éèêë]+/g, 'e');
  myString = myString.replace(/[îïìí]+/g, 'i');
  myString = myString.replace(/[òóôö]+/g, 'o');
  myString = myString.replace(/[ùúûü]+/g, 'u');
  return myString;
}

var sortByScore = function(city1, city2) {
  var comparison = 0;
  if (city1.score > city2.score) {
    comparison = -1;
  } else if (city1.score < city2.score) {
    comparison = 1;
  }
  return comparison;
}

var calcDistance = function(latitude1, longitude1, latitude2, longitude2, unit) {
  unit = (unit)? unit: 'K';
	var radlatitude1 = Math.PI * latitude1/180
	var radlatitude2 = Math.PI * latitude2/180
	var theta = longitude1-longitude2
	var radtheta = Math.PI * theta/180
	var dist = Math.sin(radlatitude1) * Math.sin(radlatitude2) + Math.cos(radlatitude1) * Math.cos(radlatitude2) * Math.cos(radtheta);
	dist = Math.acos(dist)
	dist = dist * 180/Math.PI
	dist = dist * 60 * 1.1515
	if (unit=="K") { dist = dist * 1.609344 }
  if (unit=="N") { dist = dist * 0.8684 }
	return dist
}

var calcScoreFromDistance = function(distance){
  var score = 0;
  if( distance < 10000){
    score = Math.round( (1 - (distance/1000) ) *10  ) / 10;
  }
  return score;
}

var calcScoreFromVariableLength = function(myString, locationName){
  var cityName = locationName.split(',')[0];
  var score = (myString.length*100)/cityName.length;
  return Math.round((score/100)*10)/10;
}

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
