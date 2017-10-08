const http = require("http");
const express = require("express");
const app = express();
const removeAccents = require("remove-accents");
const outputSuggestion = require("./output-suggestion.js");
const jsonQuery = require("json-query");
const data = require("./data/cities_canada-usa.json");

const cache = require("express-redis-cache")({
  host: "redis-18604.c10.us-east-1-4.ec2.cloud.redislabs.com", 
  port: 18604,
  auth_pass: "B4NyN8D0kMWM9rlc",
});
const port = process.env.PORT || 2345;

app.get("/suggestions", cache.route({ expire: 60 * 10 }),  function (req, res) {
  const city = req.query.q !== undefined ? removeAccents(req.query.q) : "";
  const longitude = req.query.longitude ? req.query.longitude : null;
  const latitude = req.query.latitude ? req.query.latitude : null;
  if( city ){
    const searchResults = jsonQuery(`cities[*asciiname~/\\b(${city})/i]`, {
      data,
      allowRegexp: true
    }).value;

    const suggestions = outputSuggestion(searchResults, longitude, latitude, city);

    if(suggestions.length > 0){
      res.status(200)
      res.json( {
        suggestions
      } );
    } else {
      res.status(404).json(
        {
         suggestions: []
       } 
     );
    }
    
  } else{
    res.status(404).json(
       {
        suggestions: []
      } 
    );
  }
});

app.get("*", (req, res) => {  res.status(404).json({})  });

module.exports = http.createServer(app).listen(port);

console.log("Server running at http://127.0.0.1:%d/suggestions", port);