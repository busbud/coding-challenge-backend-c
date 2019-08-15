var http = require('http');
var url = require('url');
var port = process.env.PORT || 2345;

var fs = require('fs');

var cities = {};
var partialCities = {};

var csvData = fs.readFileSync('data/cities_canada-usa.tsv', 'utf-8');
var csvLines = csvData.split('\n');
var headers = csvLines.shift().split('\t');

function addOrAppend(dict, key, val) {
    if (dict[key]) {
        dict[key].push(val);
    } else {
        dict[key] = [val];
    }
}

csvLines.forEach(function(line){
    row = line.split('\t');
    cityName = String(row[1])
    addOrAppend(cities, cityName, row);
    for (var i=1; i<cityName.length; i++) {
        addOrAppend(partialCities, cityName.substring(0,i), row);
    }
})

module.exports = http.createServer(function (req, res) {
  res.writeHead(404, {'Content-Type': 'text/plain'});

  if (req.url.indexOf('/suggestions') === 0) {
    queryParams = url.parse(req.url, true).query;
    if (queryParams.q) {
        if (cities[queryParams.q]) {
            console.log(cities[queryParams.q]);
        }
        if (partialCities[queryParams.q]) {
            console.log("-----------")
            console.log(partialCities[queryParams.q])
        }
    }
    res.end(JSON.stringify({
      suggestions: []
    }));
  } else {
    res.end();
  }
}).listen(port, '127.0.0.1');

console.log('Server running at http://127.0.0.1:%d/suggestions', port);