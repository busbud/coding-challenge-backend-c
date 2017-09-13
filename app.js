var http = require('http');
var express = require('express');
var request = require('request');
var app = express();
var outputSuggestion = require("./output-suggestion.js");
var port = process.env.PORT || 2345;

app.get('/suggestions', function(req, res){
  var country = req.query.q;
  var longitude = req.query.longitude ? req.query.longitude : null;
  var latitude = req.query.latitude ? req.query.latitude : null;
  var suggestions = [];
  if( country ){
    var geonames = 'http://api.geonames.org/searchJSON?name_startsWith=' + country + '&country=US&country=CA&cities=cities5000&username=crondinini&style=LONG&featureClass=P&lang=en';
    request(geonames, function (error, response, body) {
      res.status(200)
      var searchResults = JSON.parse(body);
      searchResults = searchResults["geonames"];
      var suggestionResults = outputSuggestion(searchResults, longitude, latitude, country);
    
      res.json( {
        suggestions: suggestionResults
      } );
    });
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

app.listen(port);

module.exports = http.createServer(app).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);