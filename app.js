var http = require('http');
var express = require('express');
var app = express();
var removeAccents = require("remove-accents");
var outputSuggestion = require("./output-suggestion.js");
// var outputSuggestion = require("./convert-data.js");

var cache = require('express-redis-cache')({
  host: 'redis-18604.c10.us-east-1-4.ec2.cloud.redislabs.com', 
  port: 18604,
  auth_pass: 'B4NyN8D0kMWM9rlc',
});

var jsonQuery = require('json-query');
var data = require('./data/cities_canada-usa.json');

var port = process.env.PORT || 2345;

app.get('/suggestions', cache.route({ expire: 60 * 10 }),  function(req, res){
  var city = req.query.q !== undefined ? removeAccents(req.query.q) : "";
  var longitude = req.query.longitude ? req.query.longitude : null;
  var latitude = req.query.latitude ? req.query.latitude : null;
  var suggestions = [];
  if( city ){
    var searchResults = jsonQuery('cities[*asciiname~/^' + city + '/i]', {
      data: data,
      allowRegexp: true
    }).value;

    var suggestionResults = outputSuggestion(searchResults, longitude, latitude, city);

    if(suggestionResults.length > 0){
      res.status(200)
      res.json( {
        suggestions: suggestionResults
      } );
    } else {
      res.status(404).json(
        {
         suggestions: suggestions
       } 
     );
    }
    
  } else{
    res.status(404).json(
       {
        suggestions: suggestions
      } 
    );
  }
  
})

app.get('*', function(req, res){
  res.status(404).json({})
});

module.exports = http.createServer(app).listen(port);

console.log('Server running at http://127.0.0.1:%d/suggestions', port);