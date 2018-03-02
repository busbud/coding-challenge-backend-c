var port = process.env.PORT || 2345;
var host = process.env.IP;
var fs = require("fs");
var path = require("path");
var pathExists = require("path-exists");
var express = require("express");
var app = express();

var outputPath = path.resolve(__dirname + "/data/cities_canada-usa.json");
var inputPath = path.resolve(__dirname + "/data/cities_canada-usa.tsv");


var tsv2JSON = require("./app/tsv2json");
var store = require("./app/store");

var defaultLat = "45.5017";
var defaultLn = "-73.58781";

//convert the tsv file to json
pathExists(outputPath).then(function(exists) {
  if (!exists) {
    //if the json file doesn't exist, create it and initialize the data store
    tsv2JSON(inputPath, "cities_canada-usa").then(function(msg) {
      fs.readFile(outputPath, "utf8", function(err, cities) {
        if (err) {
          throw new Error("Could not read data file");
        }
        store.init(JSON.parse(cities));
      });
    });
  }
  //the file exists, so only need to initialize the store
  else {
    fs.readFile(outputPath, "utf8", function(err, cities) {
      if (err) {
        throw new Error("Could not read data file");
      }
      store.init(JSON.parse(cities));
    });
  }
});

app.use(function restrictAccess(req, res, next) {
  if (req.url.match("suggestions")) {
    next();
  }
  else {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end();
  }
});

app.get("/suggestions", function(req, res) {
  var query = req.query;
  //if no query parameter provided, send 404
  if (!query.q) {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end();
  }
  //query the store for data. Default geo info is for Montreal
  var suggestions = store.query({
    term: query.q,
    longitude: (query.longitude ? query.longitude : defaultLn),
    latitude: (query.latitude ? query.latitude : defaultLat)
  });

  //if no data returned from store, send a 404 with
  //an empty suggestion array
  if (suggestions.length == 0) {

    res.writeHead(404, { 'Content-Type': 'text/plain' });
    res.end(JSON.stringify({
      suggestions: []
    }));

  }
  else {

    res.writeHead(200, { 'Content-Type': 'text/plain' });
    res.end(JSON.stringify({
      suggestions: suggestions
    }));

  }
});


app.listen(port);

module.exports = app;

console.log('Server running at http://%s:%d/suggestions', host, port);
