var http = require('http');
var express = require('express');
var app = express();
var removeAccents = require("remove-accents");
var outputSuggestion = require("./output-suggestion.js");
// var outputSuggestion = require("./convert-data.js");

var jsonQuery = require('json-query')
var data = require('./data/cities_canada-usa.json');

var port = process.env.PORT || 2345;

app.get('/suggestions', function(req, res){
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

app.listen(port);

module.exports = http.createServer(app).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);