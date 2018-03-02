var http = require("http");
var port = process.env.PORT || 2345;
var host = process.env.IP;
var fs = require("fs");
var path = require("path");
var pathExists = require("path-exists");
var url = require("url");

var outputPath = path.resolve(__dirname + "/data/cities_canada-usa.json");
var inputPath = path.resolve(__dirname + "/data/cities_canada-usa.tsv");


var tsv2JSON = require("./app/tsv2json");
var store = require("./app/store");

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
      })
    })
  }
  //the file exists, so only need to initialize the store
  else {
    fs.readFile(outputPath, "utf8", function(err, cities) {
      if (err) {
        throw new Error("Could not read data file");
      }
      store.init(JSON.parse(cities));
    })
  }
});

module.exports = http.createServer(function(req, res) {
  var defaultLat = "45.5017";
  var defaultLn  = "-73.58781";
  
  if (req.url.indexOf('/suggestions') === 0) {
    //parse query string
    var query = url.parse(req.url, true).query;
    
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
  }
  else {
    res.writeHead(404, { 'Content-Type': 'text/plain' });
  }
}).listen(port, host);


console.log('Server running at http://%s:%d/suggestions', host, port);
