var http = require('http');
var port = process.env.PORT || 2345;
var url = require('url');
var Parser = require('./parser');
var mongoose = require('mongoose');
var City = require('./models/cities');

// Usuallly use .env file for username and password
var db = mongoose.connect('mongodb://busbud:busbud@ds041939.mlab.com:41939/suggestions');

module.exports = http.createServer(function (req, res) {

  if (req.url.indexOf('/suggestions') === 0) {
    City.findOne({}, function(err, doc){
      if(err){
        // Database error
        res.writeHead(404, {'Content-Type': 'application/json'});
        res.end(error(err));
      } else {
        // Read data from file
        if(!doc || doc.length == 0){
          Parser.readFile();
          // TODO Thread.sleep(15000);
          // let the database be filled
        }
        // Take params from query
        var query = url.parse(req.url, true).query;
        var results = [];

        if(query.q){
          // Find in the database all cities matching the prefix from the query
          City.find({
            // regular expression matches all prefixes
            ascii: {
              $regex: '\\b' + query.q,
              $options: 'i'
            }
          }, function(err, docs){
            // Pass callback results
            results = docs;
            if(results.length == 0){
              // If query does not match any city
              res.writeHead(404, {'Content-Type': 'application/json'});
              res.end(error("No suggestions were found"));
            } else {
              // Check for latitude and longitude in query to calculate score
              res.writeHead(200, {'Content-Type': 'application/json'});
              if(query.latitude && query.longitude){
                results = calculateScores(results, query.latitude, query.longitude);
              } else {
                // Montreal coords are default
                results = calculateScores(results, 45.5017, -73.5673);
              }
              // Send the results
              res.end(success(results));
            }
          });
        } else {
          // Not giving the parameter 'q'
          res.writeHead(404, {'Content-Type': 'application/json'});
          res.end(error("Incomplete query. Be sure to add a value for q in GET request."));
        }
      }
    })
  } else {
    // Not hitting the right endpoint
    res.writeHead(404, {'Content-Type': 'application/json'});
    res.end(error("Try /suggestions"));
  }
}).listen(port, '0.0.0.0');

var calculateScores = function(results, lat, long){
  var cities = [];
  // Construct the results object
  for(var i = 0; i < results.length; i++){
    var cityObject = {};
    cityObject.name = beautifulString(results[i]);
    cityObject.score = distanceScore(results[i], lat, long);
//    if(cityObject.score == 0) continue;
    cityObject.latitude = "" + results[i].lat;
    cityObject.longitude = "" + results[i].long;
    cities.push(cityObject);
  }
  // Sort in decreasing order of score and alphabetical order
  cities.sort(function (a,b){
    if(a.score > b.score){
      return -1;
    } else if(a.score < b.score){
      return 1;
    } else { // a.score == b.score
      if(a.name > b.name){
        return 1;
      } else if(a.name < b.name){
        return -1;
      }
      return 0;
    }
  });
  return cities;
}

// Very simple score algorithm
var distanceScore = function(city, lat, long){
  // c = sqrt(a^2 + b^2)
  var distance = Math.sqrt(Math.pow(Math.abs(lat - city.lat), 2)
                          + Math.pow(Math.abs(long - city.long), 2));
  var score = 0;
  if(distance < 0.5) score = 1.0
  if(distance < 1) score = 0.9;
  else if(distance < 2) score = 0.8
  else if(distance < 3) score = 0.7;
  else if(distance < 5) score = 0.6;
  else if(distance < 7) score = 0.5;
  else if(distance < 9) score = 0.4;
  else if(distance < 11) score = 0.3;
  else if(distance < 13) score = 0.2;
  else if(distance < 15) score = 0.1;
  return score;
}

// Construct pretty string
var beautifulString = function(city){
  var beautiful = city.ascii + ", " + city.state + ", ";
  if(city.country === 'US') beautiful += "USA";
  else if(city.country === 'CA') beautiful += "Canada";
  else beautiful += "Nowhere Land";
  return beautiful;
}

var success = function(suggestions){
  return JSON.stringify({
    suggestions: suggestions
  });
}

var error = function(errorMessage){
  return JSON.stringify({
    suggestions: [],
    error: errorMessage
  });
}

console.log('Server running at http://127.0.0.1:%d/suggestions', port);
