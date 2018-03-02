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

pathExists(outputPath).then(function(exists) {
  if (!exists) {
    tsv2JSON(inputPath, "cities_canada-usa").then(function(msg) {
      fs.readFile(outputPath, "utf8", function(err, cities) {
        store.init(JSON.parse(cities));
      })
    })
  }
  else {
    fs.readFile(outputPath, "utf8", function(err, cities) {
      store.init(JSON.parse(cities));
    })
  }
});

module.exports = http.createServer(function(req, res) {
  if (req.url.indexOf('/suggestions') === 0) {
    var query = url.parse(req.url, true).query;
    var suggestions = store.query({ 
      term: query.q, 
      longitude: (query.longitude ? query.longitude : "-73.58781"), 
      latitude: (query.latitude ? query.latitude : "45.5017")
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
