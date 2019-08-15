// TODO: calculate distance from user lat, long
// estimate is good enough
// http://jonisalonen.com/2014/computing-distance-between-coordinates-can-be-simple-and-fast/
// https://stackoverflow.com/a/15737369
// TODO: user metro area lookup? bounding box? voronoi map?
// for each city, find nearest city. bounding box vertex goes midway (prorate
// by population?) on line connecting two cities. if user lat,long are in city
// bounding box, city gets a score bump (how much?)
// TODO: partialCities has many keys -> same val; custom dict to reduce size
// https://codereview.stackexchange.com/questions/85842/a-dictionary-that-allows-multiple-keys-for-one-value
// TODO: handle typos in city query
// this is hard: https://medium.com/snipette/types-of-typos-aeee34ab0424
// most queries from phone? no keyboard clustering assumptions
// supported languages? each language has its own list of common typos
// TODO: scoring function?!? score = max(1, f(nameScore, distanceScore))
// TODO: nameScore = 1 for perfect match, 1 - n * delta
//                      where n distance from perfect match
// TODO: distance score = exp(-1 * distance from city center)
// TODO:
//
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